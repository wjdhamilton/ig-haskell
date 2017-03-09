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

module IG.Realtime where

import Control.Concurrent
import Control.Concurrent.STM as STM
import Control.Concurrent.STM.TChan as Chan
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.ByteString.Char8 as BS hiding (last)
import qualified Data.ByteString.Lazy.Char8 as BL hiding (last)
import Data.Monoid
import Data.Text as Text hiding (last)
import IG
import IG.REST
import IG.REST.Login
import Network.HTTP.Client as Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (Status, statusCode)
import Safe
import Data.Text.Encoding as TE

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
            -> IO(Either RealTimeError (TChan Text))
openSession headers host = do
  state <- runEitherT $ do
    eStream <- lift $ buildRequest headers host >>= openStream 
    stream <- hoistEither eStream
    let body = Client.responseBody $ stream
    session <- lift $ brRead body
    let (id, sessUrl) = sessionData session
    chan <- lift (atomically $ do newTChan)
    lift $ receiveData body id sessUrl host chan
    return chan
  return $ state


buildRequest :: AuthHeaders -> RealTimeURL -> IO Request
buildRequest headers url = do
  initialStream <- parseRequest . streamUrl url createSess . mkRealtimeAuth $ headers
  return $ initialStream { method = "POST" }


streamUrl :: Text -> Text -> Text -> String
streamUrl host path args = 
  Text.unpack $ realTimeBase host <> path <> "?" <> args


mkRealtimeAuth :: AuthHeaders -> Text
mkRealtimeAuth (AuthHeaders cst xst _ _) = key <> "=" <> tokens
  where key = "LS_password"
        tokens = "CST-" <> cst <> "|" <> "XST-" <> xst


realTimeBase :: Text -> Text
realTimeBase host = host <> "/lightstreamer"


openStream :: Request -> IO (Either RealTimeError (Response BodyReader))
openStream req = do
  manager <- newManager tlsManagerSettings
  realTimeRequest req manager


-- | Open a connection to the IG real time sever. If an error occurs then Left
-- RealTimeError is returned, otherwise Right (Response BodyReader)
realTimeRequest :: Request -> Manager -> IO (Either RealTimeError (Response BodyReader))
realTimeRequest req man = do
  response <- responseOpen req man
  let body = Client.responseBody response
  handshake <- brRead body
  print handshake
  case statusCode . responseStatus $ response of
       200 -> return $ case lsResponseCode handshake of
                            "OK" -> Right response
                            "1"  -> Left InvalidLogin
                            "2"  -> Left UnavailableAdapterSet
                            "7"  -> Left LicensedMaxSessionsReached
                            "8"  -> Left ConfiguredMaxSessionsReached
                            "9"  -> Left ConfiguredMaxServerLoadReached
                            "10" -> Left NewSessionsBlocked
                            "11" -> Left StreamingUnavailable
                            "60" -> Left ClientVersionNotSupported
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
                   | Other Status
                   deriving (Show)


lsResponseCode :: BS.ByteString -> Text
lsResponseCode body = Prelude.head . splitOn "\r\n" . Text.strip . TE.decodeUtf8 $ body

data FeedState = Empty


receiveData :: BodyReader -> Text -> Text -> Text -> (TChan Text) -> IO ()
receiveData body sess_id sess_url host channel = do
  datum <- brRead body
  case datum of 
       "" -> do rebindSession sess_id sess_url host channel
       _  -> do
              atomically $ writeTChan channel (TE.decodeUtf8 datum)
              receiveData body sess_id sess_url host channel


rebindSession :: Text -> Text -> Text -> TChan Text -> IO ()
rebindSession id sess_url host channel = do
  let url = streamUrl host rebindSess ("LS_session=" <> id)
  eResponse <- parseRequest url >>= openStream
  case eResponse of
       Left e -> do Safe.abort (show e)
       Right response -> do
         let body = Client.responseBody response
         receiveData body id sess_url host channel



sessionData :: BS.ByteString -> (Text, Text)
sessionData r = (sess_id, sess_url) 
  where body = Text.lines . Text.pack . BS.unpack $ r
        extractArg = last . splitOn ":" . Text.strip 
        sess_id  = case atMay body 1 of
                        Just id -> 
                          extractArg id
                        Nothing -> 
                          Safe.abort $ errorMessage "Session id could not be extracted" 
        sess_url = "https://" <> case atMay body 2 of
                                      Just url -> 
                                        url
                                      Nothing -> 
                                        errorMessage "Session url could not be extracted" 
        errorMessage s = Safe.abort (s <> " " <> (show body))

