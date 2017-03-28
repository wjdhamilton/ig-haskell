-- | This module provides functions for connecting to, manipulating, and
-- receiving data from IG's real-time data feed. 
-- There are essentially three problems to solve: How to connect, how to
-- receive the data, and how to manipulate the feed. 
--
-- The subscribing is reasonably simple. To do this, we log in to the api to
-- obtain our CST and XST tokens. We then use those to log into the
-- lightstreamer server and obtain the session Id and session url. 
--
-- Having obtained the sessionId and sessionUrl we then pass the remainder of 
-- the stream to a new thread which reads the incoming data from the
-- Lighstreamer server as it arrives.
--
-- Once the data has been received, this thread then places that data into a
-- memory structure such as an MVar which can be read by another thread and so
-- the data exported elsewhere in the application.
--
-- Manipulating the feed is a matter of updating the server using the session
-- id and session url using a control connection. The control connection is run
-- from a different thread than that which consumes the subscription. 

module IG.Realtime  where

import Control.Concurrent
import Control.Concurrent.STM as STM
import Control.Concurrent.STM.TChan as Chan
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.ByteString.Char8 as BS hiding (last)
import qualified Data.ByteString.Lazy.Char8 as BL hiding (last)
import Data.List as List
import Data.Maybe
import Data.Monoid
import Data.Text as Text hiding (last)
import Data.Text.Encoding as TE
import IG
import IG.REST
import IG.REST.Login
import IG.Realtime.Types
import Network.HTTP.Client as Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (Status, statusCode)
import qualified Network.Wreq  as Wreq
import Safe
import System.Timeout

type SessId = Text
type SessURL = Text
type RealTimeURL = Text

-- | Url segment to create a session
createSess = "/create_session.txt"

-- | Url segment to rebind to a session
rebindSess = "/bind_session.txt"

-- | Open a new session with IG Streaming API. This process requires a set of
-- AuthHeaders obtained through the IG.Login module. The function returns a
-- tuple containing the session id, the session url, and the channel to which 
-- the data feed will be written
openSession :: AuthHeaders 
            -> RealTimeURL 
            -> IO(Either RealTimeError (SessId, SessURL, TChan Text))
openSession headers host =
 runEitherT $ do
    eStream <- lift $ buildRequest headers host >>= openStream 
    (id, sessUrl, time, stream) <- hoistEither eStream
    chan <- lift (atomically newTChan)
    lift $ forkIO (listen stream id sessUrl host chan time)
    return (id, sessUrl, chan)


buildRequest :: AuthHeaders -> RealTimeURL -> IO Request
buildRequest headers url = do
  initialStream <- parseRequest . streamUrl url createSess . mkRealtimeAuth $ headers
  return $ initialStream { method = "POST" }


streamUrl :: Text -> Text -> Text -> String
streamUrl host path args = 
  Text.unpack $ realTimeBase host <> path <> "?" <> args


mkRealtimeAuth :: AuthHeaders -> Text
mkRealtimeAuth (AuthHeaders cst xst _ _) = key <> "=" <> tokens cst xst
  where key = "LS_password"


tokens :: Text -> Text -> Text
tokens cst xst = "CST-" <> cst <> "|" <> "XST-" <> xst


realTimeBase :: Text -> Text
realTimeBase host = host <> "/lightstreamer"


openStream :: Request -> IO (Either RealTimeError (Text, Text, Int, Response BodyReader))
openStream req = do
  manager <- newManager tlsManagerSettings
  realTimeRequest req manager


-- | Open a connection to the IG real time sever. If an error occurs then Left
-- RealTimeError is returned, otherwise Right (Response BodyReader)
realTimeRequest :: Request -> Manager -> IO (Either RealTimeError (Text, Text, Int, Response BodyReader))
realTimeRequest req man = do
  response <- responseOpen req man
  checkStatus response (\body -> let (id, url, timeout) = sessionData body in
                                     (id, url, timeout, response))


checkStatus :: Response BodyReader -> (ByteString -> a) -> IO (Either RealTimeError a)
checkStatus response f = do
  let body = Client.responseBody response
  handshake <- brRead body
  case statusCode . responseStatus $ response of
       200 -> return $ mapErrorToResponseCode (BL.fromStrict handshake) (f handshake)
       _   -> return $ Left (Other $ responseStatus response)


data RealTimeError = InvalidLogin
                   | UnavailableAdapterSet
                   | LicensedMaxSessionsReached
                   | ConfiguredMaxSessionsReached
                   | ConfiguredMaxServerLoadReached
                   | NewSessionsBlocked
                   | StreamingUnavailable
                   | MetadataAdapterError
                   | ClientVersionNotSupported
                   | DataAdapterUnknown
                   | TableNotFound
                   | BadItemGroupName
                   | BadItemGroupNameForSchema
                   | BadFieldSchema
                   | SubscriptionModeNotAllowed
                   | BadSelectorName
                   | NoUnfilteredDispatchFreq
                   | NoUnfilteredDispatchPre
                   | NoUnfilteredDispatch
                   | RawModeNotAllowed
                   | SubscriptionsNotAllowed
                   | SubscriptionRefused
                   | Other Status
                   deriving (Eq, Show)


mapErrorToResponseCode :: BL.ByteString -> a -> Either RealTimeError a
mapErrorToResponseCode s x = let code = lsResponseCode (BL.toStrict s) in
                             case code of
                                  "OK" -> Right $ x
                                  "1"  -> Left InvalidLogin
                                  "2"  -> Left UnavailableAdapterSet
                                  "7"  -> Left LicensedMaxSessionsReached
                                  "8"  -> Left ConfiguredMaxSessionsReached
                                  "9"  -> Left ConfiguredMaxServerLoadReached
                                  "10" -> Left NewSessionsBlocked
                                  "11" -> Left StreamingUnavailable
                                  "17" -> Left DataAdapterUnknown
                                  "19" -> Left TableNotFound 
                                  "21" -> Left BadItemGroupName
                                  "22" -> Left BadItemGroupNameForSchema
                                  "23" -> Left BadFieldSchema
                                  "24" -> Left SubscriptionModeNotAllowed
                                  "25" -> Left BadSelectorName
                                  "26" -> Left NoUnfilteredDispatchFreq
                                  "27" -> Left NoUnfilteredDispatchPre
                                  "28" -> Left NoUnfilteredDispatch
                                  "29" -> Left RawModeNotAllowed
                                  "30" -> Left SubscriptionsNotAllowed
                                  "60" -> Left ClientVersionNotSupported


lsResponseCode :: BS.ByteString -> Text
lsResponseCode body = case Prelude.head readError of
                           "OK" -> "OK"
                           "ERROR" -> List.last $ List.take 2 readError
  where readError = splitOn "\r\n" . Text.strip . TE.decodeUtf8 $ body


data FeedState = Loop
               | Probe
               | End
               | Datum ByteString


timeoutFlag = "TIMEOUT"


-- TODO: Need to close the response once it's finished or no longer required.
listen :: Response BodyReader -> Text -> Text -> Text -> TChan Text -> Int -> IO ()
listen response sess_id sess_url host channel time = do
  let body = Client.responseBody response
  let t = time * 1000
  mDatum <- timeout t (brRead body)
  case mDatum of 
       Nothing ->
         atomically $ do writeTChan channel timeoutFlag
       Just datum ->
         case getFeedState datum of 
              Loop    ->  
                rebindSession sess_id sess_url host channel time
              End     -> do
                atomically $ writeTChan channel "Stream exhausted"
                responseClose response
              Probe   -> do
                listen response sess_id sess_url host channel time
              Datum d -> do
                atomically $ writeTChan channel (TE.decodeUtf8 datum)
                listen response sess_id sess_url host channel time


getFeedState :: ByteString -> FeedState
getFeedState content = 
  let message = lsResponseCode content in
  case message of
       "PROBE" -> Probe
       "END"   -> End
       "LOOP"  -> Loop
       _       -> Datum content


rebindSession :: Text -> Text -> Text -> TChan Text -> Int -> IO ()
rebindSession id sess_url host channel timeout = do
  let url = streamUrl host rebindSess ("LS_session=" <> id)
  eResponse <- parseRequest url >>= openStream
  case eResponse of
       Left e -> Safe.abort (show e)
       Right (_,_,_,response) -> do
         listen response id sess_url host channel timeout



sessionData :: BS.ByteString -> (Text, Text, Int)
sessionData r = (sess_id, sess_url, timeout) 
  where body = Text.lines . Text.pack . BS.unpack $ r
        extractArg = last . splitOn ":" . Text.strip 
        sess_id  = case atMay body 1 of
                        Just id -> 
                          extractArg id
                        Nothing -> 
                          errorMessage "Session id could not be decoded" 
        url = fromMaybe (errorMessage "Session url could not be decoded") (atMay body 2)
        sess_url = "https://" <> extractArg url
        timeout = fromMaybe (errorMessage "Timeout could not be decoded") 
                            (atMay body 3 >>= (\s -> readMay $ Text.unpack . extractArg $ s))
        errorMessage s = Safe.abort (s <> " " <> show body)


{--------------------------- Control Messages ----------------------------------}


control :: Text -> [ControlProperty] -> Schema -> IO (Either RealTimeError ())
control url atts schema = do
  let opts = Wreq.defaults & Wreq.header "Content-Type" .~ ["application/x-www-form-urlencoded"]
  let payload = lsBody atts schema
  let controlUrl = Text.unpack $ url <> "/lightstreamer/control.txt"
  response <- Wreq.postWith opts controlUrl payload
  let body = response ^. Wreq.responseBody
  let status = response ^. Wreq.responseStatus 
  case status ^. Wreq.statusCode of
       200 -> return $ mapErrorToResponseCode body ()
       _ -> return $ Left (Other status)


lsBody :: [ControlProperty] -> Schema -> [Wreq.FormParam]
lsBody atts schema = encodeSchema schema ++ List.map encodeProperty atts
