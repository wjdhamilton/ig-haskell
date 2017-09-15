module IG.REST.Accounts where

import Data.Aeson.Types 
import Data.String.Conversions
import IG
import IG.REST
import IG.REST.Accounts.Types
import Network.Wreq



getAccounts :: AuthHeaders -> IO (Either ApiError [Account])
getAccounts a@(AuthHeaders _ _ _ isDemo) = do
  let opts = v1 a
  let url = cs $ host isDemo </> "gateway" </> "deal" </> "accounts"
  response <- apiRequest $ getWith opts url :: IO (Either ApiError Value)
  case response of
       Left e -> return $ Left e
       Right r ->
         let
           decodeAccounts :: Value -> Parser [Account]
           decodeAccounts = withObject "accounts" $ \o -> o .: "accounts"
         in
           case parseEither decodeAccounts r of
                Left parseError -> return (Left . BadPayload . cs $ parseError)
                Right details -> return $ Right details

