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
-- TChan StreamContent which can be read by another thread and so the data exported
-- elsewhere in the application.
--
-- Manipulating the feed is a matter of updating the server using the session
-- id and session url using a control connection. The control connection is run
-- from a different thread than that which consumes the subscription. 

module IG.Realtime (openSession, openSessionWith, control, RealTimeError(..), SessId, SessURL, RealTimeURL) where

import Control.Concurrent
import Control.Concurrent.STM as STM
import Control.Lens hiding ((|>))
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS hiding (last)
import qualified Data.ByteString.Lazy.Char8 as BL hiding (last)
import Data.List as List
import Data.Maybe
import Data.Monoid
import Data.String.Conversions
import Data.Text (Text)
import Data.Time.Clock.POSIX
import qualified Data.Text as Text 
import IG.REST
import IG.Realtime.Types
import Network.HTTP.Client (BodyReader, Manager, Response, Request)
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import qualified Network.Wreq  as Wreq
import Safe
import qualified System.Timeout as TO


type SessId = Text
type SessURL = Text
type RealTimeURL = Text

-- | Url segment to create a session
createSess :: Text
createSess = "/create_session.txt"

-- | Url segment to rebind to a session
rebindSess :: Text
rebindSess = "/bind_session.txt"

-- | Open a new session with IG Streaming API. This process requires a set of
-- AuthHeaders obtained through the IG.Login module. The function returns a
-- tuple containing the session id, the session url, and the channel to which 
-- the data feed will be written
openSession :: AuthHeaders 
            -> RealTimeURL 
            -> IO(Either RealTimeError (SessId, SessURL, TChan StreamContent))
openSession headers rtHost =
 openSessionWith headers [] rtHost


openSessionWith :: AuthHeaders 
                -> [StreamProperty] 
                -> RealTimeURL
                -> IO(Either RealTimeError (SessId, SessURL, TChan StreamContent))
openSessionWith headers props rtHost =
 runEitherT $ do
   let encoded = map encodeStreamProperty props
   eStream <- lift $ buildRequest headers encoded rtHost >>= openStream 
   (ident, sessUrl, time, stream) <- hoistEither eStream
   chan <- lift (atomically newTChan)
   lift $ forkIO (listen stream ident rtHost chan time)
   lift $ heartBeat sessUrl ident
   return (ident, sessUrl, chan)


buildRequest :: AuthHeaders -> [Text] -> RealTimeURL -> IO Request
buildRequest headers queries url = do
  let qString = mconcat . List.intersperse "&" $ (mkRealtimeAuth headers : queries)
  initialStream <- Client.parseRequest (streamUrl url createSess qString)
  return $ initialStream { Client.method = "POST" }


streamUrl :: Text -> Text -> Text -> String
streamUrl rtHost path query = 
  cs $ realTimeBase rtHost <> path <> "?" <> query


mkRealtimeAuth :: AuthHeaders -> Text
mkRealtimeAuth (AuthHeaders cst xt _ _) = mconcat [key, "=", tokens cst xt]
  where key = "LS_password"


tokens :: Text -> Text -> Text
tokens cst xst = "CST-" <> cst <> "|" <> "XST-" <> xst


realTimeBase :: Text -> Text
realTimeBase rtHost = rtHost <> "/lightstreamer"


openStream :: Request -> IO (Either RealTimeError (Text, Text, Int, Response BodyReader))
openStream req = do
  manager <- Client.newManager tlsManagerSettings
  realTimeRequest req manager


-- | Open a connection to the IG real time sever. If an error occurs then Left
-- RealTimeError is returned, otherwise Right (Response BodyReader)
realTimeRequest :: Request -> Manager -> IO (Either RealTimeError (Text, Text, Int, Response BodyReader))
realTimeRequest req man = runEitherT $ lift (Client.responseOpen req man) >>= \response ->
                                       lift (checkStatus response) >>= 
                                       hoistEither >>= \s ->
                                       hoistEither (sessionData s) >>= \(ident, url, timeout) ->
                                       pure (ident, url, timeout, response)


checkStatus :: Response BodyReader -> IO (Either RealTimeError BS.ByteString)
checkStatus response = do
  let body = Client.responseBody response
  handshake <- Client.brRead body
  case statusCode . Client.responseStatus $ response of
       200 -> return $ Right handshake
       _   -> return $ Left (Other $ show . Client.responseStatus $ response)


decodeDataFeed :: BL.ByteString -> a -> Either RealTimeError a
decodeDataFeed s x = let code = lsResponseCode (BL.toStrict s) in
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
                                  "23" -> Left BadFieldSchemaName
                                  "24" -> Left SubscriptionModeNotAllowed
                                  "25" -> Left BadSelectorName
                                  "26" -> Left NoUnfilteredDispatchFreq
                                  "27" -> Left NoUnfilteredDispatchPre
                                  "28" -> Left NoUnfilteredDispatch
                                  "29" -> Left RawModeNotAllowed
                                  "30" -> Left SubscriptionsNotAllowed
                                  "60" -> Left ClientVersionNotSupported
                                  _    -> Left (Other $ cs code)


lsResponseCode :: BS.ByteString -> Text
lsResponseCode body = case message of
                           "ERROR" -> List.last $ List.take 2 readBody
                           _ -> message
  where readBody = Text.splitOn "\r\n" . Text.strip . cs $ body
        message = fromMaybe "Cannot read response code" (headMay readBody)


data FeedState = Loop
               | Probe
               | End
               | Timeout
               | SyncError
               | Datum Text


listen :: Response BodyReader -> Text -> Text -> TChan StreamContent-> Int -> IO ()
listen response sess_id rtHost channel timeoutMillis = do
  let body = Client.responseBody response
  let toMicrosecs = timeoutMillis * 1000
  mUpdate <- TO.timeout toMicrosecs (Client.brRead body)
  case mUpdate of
       Just update -> 
         case getFeedState update of 
            Loop    ->  
              rebindSession sess_id rtHost channel timeoutMillis
            End     -> do
              atomically $ writeTChan channel Exhausted
              Client.responseClose response
            Probe   -> do
              listen response sess_id rtHost channel timeoutMillis
            Datum d -> do
              let t = readTable d
              atomically $ writeTChan channel t
              listen response sess_id rtHost channel timeoutMillis
            Timeout -> 
              atomically $ writeTChan channel Timedout
            SyncError -> 
              rebindSession sess_id rtHost channel timeoutMillis
       Nothing -> do
             atomically $ writeTChan channel Timedout
             Client.responseClose response


getFeedState :: ByteString -> FeedState
getFeedState content = 
  let message = lsResponseCode content in
  case message of
       "PROBE"   -> Probe
       "END"     -> End
       "LOOP"    -> Loop
       "TIMEOUT" -> Timeout
       "SYNC ERROR" -> SyncError
       _         -> Datum $ noWhiteSpace content
  where noWhiteSpace = Text.strip . cs


rebindSession :: Text -> Text -> TChan StreamContent-> Int -> IO ()
rebindSession ident rtHost channel timeout = do
  let url = streamUrl rtHost rebindSess ("LS_session=" <> ident)
  eResponse <- Client.parseRequest url >>= openStream
  case eResponse of
       Left e -> 
         atomically $ writeTChan channel (CannotRebind e)
       Right (_,_,_,response) -> do
         listen response ident rtHost channel timeout



sessionData :: BS.ByteString -> Either RealTimeError (Text, Text, Int)
sessionData r = case (,,) <$> sess_id <*> sess_url <*> time of
                     Nothing -> Left SessionDataUnreadable
                     Just d  -> Right d
  where sess_id    = atMay body 1 >>= \iden -> Just $ extractArg iden
        sess_url   = atMay body 2 >>= \path -> Just $ "https://" <> extractArg path
        time    = atMay body 3 >>= \t -> readMay $ cs . extractArg $ t
        extractArg = last . Text.splitOn ":" . Text.strip 
        body = Text.lines . cs $ r


readTable :: Text -> StreamContent
readTable t = if tNum == (-1) then mkHeartBeat $ head (tail splut)
                              else Update $ LSValue tNum iNum values
  where splut = Text.splitOn "|" t
        nums = Text.splitOn "," (head splut)
        tNum = read $ cs (nums !! 0) :: TableNo
        iNum = read $ cs (nums !! 1) :: ItemNo
        values = decodeUpdate $ tail splut 


mkHeartBeat :: Text -> StreamContent
mkHeartBeat = Beat . posixSecondsToUTCTime . fromIntegral . read . cs


-- | Decode the update, respecting the protocol set out on page 29 of the 
-- Network Protocol Tutorial
decodeUpdate :: [Text] -> [Maybe Text]
decodeUpdate = map maybeVal
  where checkTail t = if Text.null t then Nothing else Just t
        -- TODO should this return Just v is Text.null is true?
        maybeVal v  = if Text.null v then Just v
                                     else case Text.head v of
                                               '#' -> checkTail (Text.tail v)
                                               '$' -> checkTail (Text.tail v)
                                               _ -> Just $ v


{--------------------------- Control Messages ----------------------------------}


-- | Send a control message to the Lightstreamer server
control :: SessURL -- ^ The url obtained from openSession
        -> [ControlProperty] -- ^ The list of control properties. See docs for required properties
        -> Schema -- ^ The schema to use
        -> IO (Either RealTimeError ())
control url props schema = do
  let opts = Wreq.defaults & Wreq.header "Content-Type" .~ ["application/x-www-form-urlencoded"]
  let payload = lsBody props schema
  let controlUrl = cs $ url <> "/lightstreamer/control.txt"
  -- TODO this should use try since postWith can throw an error (or is there a lib option? check)
  response <- Wreq.postWith opts controlUrl payload
  let body = response ^. Wreq.responseBody
  let status = response ^. Wreq.responseStatus 
  case status ^. Wreq.statusCode of
       200 -> return $ decodeDataFeed body ()
       _ -> return $ Left (Other $ show status)


-- | Subscribe to the heartbeat. This is in order to try and keep the connection 
-- alive and active when a data source is quiet. The Heartbeat's data is always
-- returned under table -1
heartBeat :: SessURL -> SessId -> IO (Either RealTimeError ())
heartBeat url sId = control url props schema
  where props = [ SessionId sId, Op Add, Mode Merge, Table (-1)]
        schema = Trade beatId [Heartbeat]
        beatId = "HB.U.HEARTBEAT.IP" 


lsBody :: [ControlProperty] -> Schema -> [Wreq.FormParam]
lsBody atts schema = encodeSchema schema ++ List.map encodeProperty atts
