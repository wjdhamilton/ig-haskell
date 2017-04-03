
{-# LANGUAGE OverloadedStrings #-}
module IG.REST.MarketsSpec (spec) where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Either
import Data.Either.Unwrap hiding (isRight)
import Data.Maybe
import Data.Monoid
import qualified Data.Text as Text hiding (filter, head, map)
import Data.Time.Calendar
import Data.Time.Clock
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
  describe "historical data with dates" $ historicalDataWithDatesSpec headers
  describe "historical data with max" $ historicalDataWithMax headers
  describe "historical data with page size" $ historicalDataWithPageSize headers
  describe "historical data with day resolution" $ historicalDataWithResolution headers


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


ftse = "IX.D.FTSE.DAILY.IP"

historicalDataSpec :: AuthHeaders -> Spec
historicalDataSpec h = 

   it "should download data with the default settings" $ do
     let opts = HistoryOpts Nothing Nothing Nothing Nothing Nothing Nothing
     resp <- historicalData h ftse opts
     isRight resp `shouldBe` True
    
historicalDataWithDatesSpec :: AuthHeaders -> Spec
historicalDataWithDatesSpec h = 

   it "should downlad data between two dates" $ do
     let from = UTCTime (fromGregorian 2017 3 27) 0
     let to = UTCTime (fromGregorian 2017 3 28) 0
     let opts = HistoryOpts (Just from) (Just to) Nothing Nothing Nothing Nothing
     resp <- historicalData h ftse opts
     isRight resp `shouldBe` True


historicalDataWithMax :: AuthHeaders -> Spec
historicalDataWithMax h = 

  it "should only download max daums" $ do
    let n = 13
    let opts = HistoryOpts Nothing Nothing (Just n) Nothing Nothing Nothing
    resp <- historicalData h ftse opts
    case resp of
         Left e -> fail (show e)
         Right d -> do 
          length (prices d) `shouldBe` n

historicalDataWithPageSize :: AuthHeaders -> Spec 
historicalDataWithPageSize h = 
  it "should return datums where page size is fixed" $ do 
    let n = 5
    let opts = HistoryOpts Nothing Nothing Nothing (Just n) Nothing Nothing
    resp <- historicalData h ftse opts 
    case resp of
         Left e -> fail (show e)
         Right d -> do
           length (prices d) `shouldBe` n


historicalDataWithResolution :: AuthHeaders -> Spec
historicalDataWithResolution h = 

  it "should return the data in days" $ do
    let r = HOUR
    let opts = HistoryOpts Nothing Nothing Nothing Nothing (Just r) Nothing
    resp <- historicalData h ftse opts
    case resp of
         Left e -> fail (show e)
         Right r -> do
           let points = toUTCTime . snapshotTimeUTC <$> take 2 (prices r)
           diffUTCTime (last points) (head points) `shouldBe` 3600
           


historicalDataForPage :: AuthHeaders -> Spec
historicalDataForPage h = 

  it "should return the data for the correct page" $ do
    let n = 2
    let opts = HistoryOpts Nothing Nothing (Just 1) Nothing Nothing (Just n)
    resp <- historicalData h ftse opts
    case resp of
         Left e -> fail (show e)
         Right r -> do
           let page = pageData . metadata $ r
           pageNumber (page :: PageData) `shouldBe` n

