{-# LANGUAGE OverloadedStrings, OverloadedRecordDot  #-}

module Main (main) where

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import System.FilePath (takeBaseName, replaceExtension)
import qualified Data.ByteString.Lazy as LBs
import Text.Printf (perror)
import qualified Data.Vector as V
import Data.Csv as Csv 
import Data.CreditCard
import Data.ByteString (ByteString)
import Data.String ( IsString (fromString))
import Control.Monad.Reader

-- | TestApp should provide monadic envirment to test with 
type AppEnv = ReaderT Env IO 
newtype TestApp a = MkApp { unApp :: AppEnv a } deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

main :: IO ()
main = do
  gTests <- goldenTests
  defaultMain $ testGroup "All Tests" [gTests, unitTests]

execAppEnv :: TestApp a -> IO a 
execAppEnv app = do
    env <- setEnv def
    runReaderT (unApp app) env

goldenTests :: IO TestTree
goldenTests = do
  csvFiles <- findByExtension [".csv"] "./test/tests"
  pure $ testGroup "Credit Card creation" 
    [ goldenVsString 
        (takeBaseName csvFile)          -- ^ test name
        ccFile                          -- ^ golden cc file
        (execAppEnv $ goldenTestAction csvFile)      -- ^ action which result is tested 
    | csvFile <- csvFiles
    , let ccFile = replaceExtension csvFile ".cc"      
    ]  

goldenTestAction :: FilePath -> TestApp LBs.ByteString
goldenTestAction csvFile = do
    csvData <- liftIO $ LBs.readFile csvFile
    case Csv.decode Csv.HasHeader csvData of 
      Left err -> perror $ show err
      Right v  -> mconcat . V.toList <$> V.forM v cardAction   

-- | current CSV header is:
-- CardType,Card Number, Exp. Dat, CV Code, Country/Currency, Result
cardAction :: (ByteString, ByteString, ByteString, ByteString, ByteString, ByteString) 
           -> TestApp LBs.ByteString 
cardAction (cType, cNumber, cDate, cCVC, cCountry, cResult) 
  | cCVC == "" = cardToBS (cNumber, cDate, "123", cType, cCountry, cResult)
  | otherwise = cardToBS (cNumber, cDate, cCVC, cType, cCountry, cResult) 
             
cardToBS :: (ByteString, ByteString, ByteString, ByteString, ByteString, ByteString) 
         -> TestApp LBs.ByteString
cardToBS (cNumber, cDate, cCVC, cType, cCountry, cResult) = do 
     let cName = "Test Last"
     vCard <- createCreditCard (cNumber, cName, cDate, cCVC)
     pure $ case cResult of 
       "Success" -> fromString $ mconcat ["OK - ", show cResult, " : ", show cType, " - ", show cCountry, show vCard, "\n"]
       _ -> fromString $ mconcat ["FAIL - ",show cResult, " : ", show cType, " - ", show cCountry, show vCard, "\n"]

unitTests :: TestTree
unitTests = testGroup "Public API Unit Tests"
  [ testGroup "mkCardName"
      [ testCase "Valid two names" $
          show (mkCardName "John Doe") @?= "Just (MkCardName \"John Doe\")"
      , testCase "Valid three names" $
          show (mkCardName "John Mid Doe") @?= "Just (MkCardName \"John Mid Doe\")"
      , testCase "Invalid empty" $
          show (mkCardName "") @?= "Nothing"
      , testCase "Invalid single name" $
          show (mkCardName "John") @?= "Nothing"
      ]
  , testGroup "mkCCV"
      [ testCase "Valid 3 digits" $
          show (mkCCV "123") @?= "Just ***"
      , testCase "Valid 4 digits" $
          show (mkCCV "1234") @?= "Just ***"
      , testCase "Invalid letters" $
          show (mkCCV "abc") @?= "Nothing"
      ]
  , testGroup "mkValidDate"
      [ testCase "Valid four digit year" $
          show (mkValidDate "12/2026") @?= "Just (12,2026)"
      , testCase "Valid two digit year" $
          show (mkValidDate "12/20") @?= "Just (12,20)"
      , testCase "Invalid month" $
          show (mkValidDate "13/2026") @?= "Nothing"
      , testCase "Invalid past year" $
          show (mkValidDate "12/2019") @?= "Nothing"
      ]
  , testGroup "mkCardNumber"
      [ testCase "Valid card number" $
          show (mkCardNumber "4263982640269299") @?= "Just \"426398******9299\""
      , testCase "Invalid too short" $
          show (mkCardNumber "1234567") @?= "Nothing"
      , testCase "Invalid accId too short" $
          show (mkCardNumber "30569309025904") @?= "Nothing"
      ]
  , testGroup "mkCreditCard"
      [ testCase "Valid credit card" $
          case mkCreditCard ("4263982640269299", "John Doe", "12/2026", "123") of
            Just _ -> assertBool "" True
            Nothing -> assertBool "Should be Just" False
      , testCase "Invalid credit card number" $
          case mkCreditCard ("30569309025904", "John Doe", "12/2026", "123") of
            Just _ -> assertBool "Should be Nothing" False
            Nothing -> assertBool "" True
      ]
  ]
