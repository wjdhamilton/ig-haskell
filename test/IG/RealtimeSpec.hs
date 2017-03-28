{-# LANGUAGE OverloadedStrings #-}
module IG.RealtimeSpec (spec) where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy.Char8 as BL
import Data.Either
import Data.Either.Unwrap hiding (isRight)
import Data.Maybe
import Data.Monoid
import Data.Text as Text hiding (filter, head, map)
import IG
import IG.Realtime
import IG.REST.Login
import System.IO
import System.Directory
import Test.Hspec

spec :: Spec
spec = do
  describe "error" detectErrorSpec


detectErrorSpec :: Spec
detectErrorSpec = mapM_ lsErrorSpec errors
  where f = True
        lsErrorSpec (bod,exp) = it ("detects " <> (show exp)) $
                                    mapErrorToResponseCode bod f `shouldBe` exp
        errors = [ (oK, Right f)
                 , (lsError "1",  Left InvalidLogin)
                 , (lsError "2",  Left UnavailableAdapterSet)
                 , (lsError "7",  Left LicensedMaxSessionsReached)
                 , (lsError "8",  Left ConfiguredMaxSessionsReached)
                 , (lsError "9",  Left ConfiguredMaxServerLoadReached)
                 , (lsError "10", Left NewSessionsBlocked)
                 , (lsError "11", Left StreamingUnavailable)
                 , (lsError "17", Left DataAdapterUnknown)
                 , (lsError "19", Left TableNotFound )
                 , (lsError "21", Left BadItemGroupName)
                 , (lsError "22", Left BadItemGroupNameForSchema)
                 , (lsError "23", Left BadFieldSchema)
                 , (lsError "24", Left SubscriptionModeNotAllowed)
                 , (lsError "25", Left BadSelectorName)
                 , (lsError "26", Left NoUnfilteredDispatchFreq)
                 , (lsError "27", Left NoUnfilteredDispatchPre)
                 , (lsError "28", Left NoUnfilteredDispatch)
                 , (lsError "29", Left RawModeNotAllowed)
                 , (lsError "30", Left SubscriptionsNotAllowed)
                 , (lsError "60", Left ClientVersionNotSupported)
                 ]


oK :: BL.ByteString
oK = "OK\r\n"


lsError :: BL.ByteString -> BL.ByteString
lsError code = "ERROR\r\n" <> code <> "\r\n"
