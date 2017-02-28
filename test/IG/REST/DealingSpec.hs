module IG.REST.DealingSpec (spec) where

import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Either
import Data.Monoid
import Flow
import IG
import IG.REST
import IG.REST.Dealing
import IG.REST.Dealing.Types
import Test.Hspec
import Tools

-- We need to be a little careful about the sequence of things here, I don't 
-- want to create a new position and then leave it open on the api
-- Create a position
-- Use it
-- Close it
spec :: Spec
spec = do
  (headers, loginResponse) <- loginToApi
  afterAll_ (closeAll headers) (descriptions headers)


descriptions :: AuthHeaders -> Spec
descriptions headers = do
  describe "createPosition" $ createPositionSpec headers
  describe "confirms"       $ confirmsSpec headers
  describe "allPositions"   $ allPositionsSpec headers
  describe "closePosition"  $ closePositionSpec headers
  describe "updatePosition" $ updatePositionSpec headers
  describe "workingOrders"  $ workingOrdersSpec headers

createPositionSpec :: AuthHeaders -> Spec
createPositionSpec headers = 
  it "should create the new position" $ do
    response <- createPosition headers minimalPositionRequest
    isRight response `shouldBe` True


minimalPositionRequest :: PositionRequest
minimalPositionRequest = PositionRequest { currencyCode = "GBP"
                                         , dealReference = Nothing
                                         , direction = BUY
                                         , epic = "IX.D.FTSE.DAILY.IP"
                                         , expiry = DFB
                                         , forceOpen = True
                                         , guaranteedStop = False
                                         , level = Nothing
                                         , limitDistance = Nothing
                                         , limitLevel = Nothing
                                         , orderType = MARKET
                                         , quoteId = Nothing
                                         , size = 1
                                         , stopLevel = Nothing
                                         , timeInForce = FILL_OR_KILL
                                         , trailingStop = False
                                         , trailingStopIncrement = Nothing
                                         }


confirmsSpec :: AuthHeaders -> Spec
confirmsSpec headers =
  it "should retrieve the status of the deal" $ do
    dealRef <- createPosition headers minimalPositionRequest
    case dealRef of
         Left e -> error $ "Create position failed: " <> show e
         Right ref -> do
           confirmation <- confirms headers ref
           isRight confirmation `shouldBe` True


allPositionsSpec :: AuthHeaders -> Spec
allPositionsSpec headers =
  it "should retrieve a list of all the currently open positions" $ do
    positions <- allPositions headers
    isRight positions `shouldBe` True


closePositionSpec :: AuthHeaders -> Spec
closePositionSpec headers =
  it "should close a position correctly" $ do
    closeRef <- runEitherT $ do
      eDealRef <- lift $ createPosition headers minimalPositionRequest
      ref <- hoistEither eDealRef
      confirmation <- lift $ confirms headers ref
      conf <- hoistEither confirmation
      let id = dealId (conf :: DealConfirmation)
      position <- lift $ positionDetails headers id
      pos <- hoistEither position
      let opts = CloseOptions Nothing MARKET Nothing Nothing Nothing Nothing Nothing
      lift $ closePosition headers pos opts 
    isRight closeRef `shouldBe` True


updatePositionSpec :: AuthHeaders -> Spec
updatePositionSpec headers = 
  it "should update the position correctly" $ do
    response <- runEitherT $ do
      eDealRef <- lift $ createPosition headers minimalPositionRequest
      dealRef <- hoistEither eDealRef
      eConf <- lift $ confirms headers dealRef
      confirmation <- hoistEither eConf
      let id = dealId (confirmation :: DealConfirmation)
      ePos <- lift $ positionDetails headers id
      pos <- hoistEither ePos
      let p = position (pos :: PositionData)
      let l = level (p :: Position)
      let stopLevel = Just $ l - 10
      let updateOpts = PositionUpdateRequest Nothing stopLevel (Just False) Nothing Nothing
      lift $ updatePosition headers id updateOpts
    isRight response `shouldBe` True



workingOrdersSpec :: AuthHeaders -> Spec 
workingOrdersSpec headers = 
  it "should download all the working orders" $ do
    orders <- getWorkingOrders headers
    isRight orders `shouldBe` True


closeAll :: AuthHeaders -> IO ()
closeAll h = do
  eOpenPositions <- allPositions h
  case eOpenPositions of
       Left e -> print e
       Right openPositions -> do
         let posits = positions openPositions
         mapM (closeRequest h) posits
         return ()


closeRequest :: AuthHeaders -> PositionData -> IO (Either ApiError DealReference)
closeRequest h pos = closePosition h pos options
  where options = CloseOptions Nothing MARKET Nothing Nothing Nothing Nothing Nothing
