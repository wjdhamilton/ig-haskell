
{-# LANGUAGE OverloadedStrings #-}
module IG.REST.MarketsSpec (spec) where

import Control.Monad
import Data.Aeson.Lens
import Data.Either
import Data.Either.Unwrap hiding (isRight)
import Data.Maybe
import Data.Text as Text hiding (filter, head, map)
import IG
import IG.REST
import IG.REST.Login
import IG.REST.Markets
import IG.REST.Markets.Types
import System.Directory
import Test.Hspec

spec :: Spec
spec = do
  eh <- runIO $ loginToApi True
  let (headers, _) = fromRight eh
  describe "navigateMarkets" $ navigateMarketsSpec headers 
  describe "markets" $ marketsSpec headers 
  describe "historical data" $ historicalDataSpec headers
  describe "market" $ marketSpec headers


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

  it "should make a successful request" $ do
    let epics = ["IX.D.FTSE.DAILY.IP","IX.D.SAF.DAILY.IP"]
    resp <- IG.REST.Markets.markets h epics
    isRight resp `shouldBe` True

  -- Cannot work out how to overcome "Ambiguous occurrence 'epic'" in this function
  -- it "should download the correct market" $ do
  --   let epics = ["IX.D.FTSE.DAILY.IP"]
  --   resp <- IG.REST.Markets.markets h epics
  --   case resp of
  --        Left e -> 
  --          fail $ show e
  --        Right r ->
  --          let ep = epic . instrument $ (head r :: Market) in
  --          ep == head epics `shouldBe` True


historicalDataSpec :: AuthHeaders -> Spec
historicalDataSpec h = 

 it "should download data with the default settings" $ do
   let epic = "IX.D.FTSE.DAILY.IP"
   let opts = HistoryOpts Nothing Nothing Nothing Nothing
   resp <- historicalData h epic opts Nothing
   isRight resp `shouldBe` True


marketSpec :: AuthHeaders -> Spec
marketSpec h = 

  it "should successfully perform the request" $ do
   let epic = "IX.D.FTSE.DAILY.IP"
   resp <- IG.REST.Markets.market h epic
   isRight resp `shouldBe` True
