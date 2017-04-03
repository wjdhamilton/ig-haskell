
{-# LANGUAGE OverloadedStrings #-}
module IG.REST.MarketsSpec (spec) where

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
import IG.REST
import IG.REST.Login
import IG.REST.Markets
import IG.REST.Markets.Types
import System.IO
import System.Directory
import Test.Hspec
import Tools

spec :: Spec
spec = do
  (headers, lR) <- Tools.loginToApi
  describe "navigateMarkets" $ navigateMarketsSpec headers 
  describe "markets" $ marketsSpec headers 
  describe "historical data" $ historicalDataSpec headers


navigateMarketsSpec :: AuthHeaders -> Spec
navigateMarketsSpec h = do

  it "should download the root market node if the node is empty" $ do
    resp <- navigateMarkets h Nothing
    isRight resp `shouldBe` True


  it "should download a specific market node if one is supplied" $ do
    let n = Node "97601" "Indices"
    resp <- navigateMarkets h (Just n)
    isRight resp `shouldBe` True


marketsSpec :: AuthHeaders -> Spec
marketsSpec h = do

  it "should download the details of the markets" $ do
    let epics = ["IX.D.FTSE.DAILY.IP","IX.D.SAF.DAILY.IP"]
    resp <- IG.REST.Markets.markets h epics
    isRight resp `shouldBe` True



historicalDataSpec :: AuthHeaders -> Spec
historicalDataSpec h = 

 it "should download data with the default settings" $ do
   let epic = "IX.D.FTSE.DAILY.IP"
   let opts = HistoryOpts Nothing Nothing Nothing Nothing
   resp <- historicalData h epic opts Nothing
   isRight resp `shouldBe` True
  
