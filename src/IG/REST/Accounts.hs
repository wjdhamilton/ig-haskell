module IG.REST.Accounts where

import Data.Text as Text
import Data.Aeson.Types 
import IG
import IG.REST
import IG.REST.Accounts.Types
import IG.REST.Login
import Network.Wreq



getAccounts :: AuthHeaders -> IO (Either ApiError [Account])
getAccounts a@(AuthHeaders _ _ _ isDemo) = do
  let opts = v1 a
  let url = Text.unpack $ host isDemo </> "gateway" </> "deal" </> "accounts"
  response <- apiRequest $ getWith opts url :: IO (Either ApiError Value)
  case response of
       Left e -> return $ Left e
       Right r ->
         let
           decodeAccounts :: Value -> Parser [Account]
           decodeAccounts = withObject "accounts" $ \o -> o .: "accounts"
         in
           case parseEither decodeAccounts r of
                Left parseError -> return (Left . BadPayload . Text.pack $ parseError)
                Right details -> return $ Right details

