{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main (main) where

import           Control.Monad.Reader
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Lazy          as LBs
import           Data.CreditCard
import           Data.Csv                      as Csv
import           Data.Int                      (Int8)
import           Data.List.NonEmpty            (NonEmpty (..))
import           Data.String                   (IsString (fromString))
import qualified Data.Vector                   as V
import           System.FilePath               (replaceExtension, takeBaseName)
import           Test.QuickCheck
import           Test.Tasty                    (TestTree, defaultMain, testGroup)
import           Test.Tasty.Golden             (findByExtension, goldenVsString)
import           Test.Tasty.HUnit              (assertBool, testCase, (@?=))
import           Test.Tasty.QuickCheck         (testProperty)
import           Text.Printf                   (perror)

-- | TestApp should provide monadic environment to test with 
type AppEnv = ReaderT Env IO 
newtype TestApp a = MkApp { unApp :: AppEnv a } deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

main :: IO ()
main = do
  gTests <- goldenTests
  defaultMain $ testGroup "All Tests" [gTests, unitTests, propertyTests]

execAppEnv :: TestApp a -> IO a 
execAppEnv app = do
  env <- setEnv def
  runReaderT (unApp app) env

goldenTests :: IO TestTree
goldenTests = do
  csvFiles <- findByExtension [".csv"] "./test/tests"
  pure $ testGroup "Credit Card creation" 
    [ goldenVsString 
        (takeBaseName csvFile)
        ccFile
        (execAppEnv $ goldenTestAction csvFile)
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
  vCard <- create [cNumber, cName, cDate, cCVC, cCountry]
  pure $ case cResult of 
    "Success" -> fromString $ mconcat ["OK - ", show cResult, " : ", show cType, " - ", show cCountry, show vCard, "\n"]
    _ -> fromString $ mconcat ["FAIL - ", show cResult, " : ", show cType, " - ", show cCountry, show vCard, "\n"]

propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [ testProperty "create is invariant under permutation of input list" prop_create_permutation_invariant
  ]

-- | Known valid card field combinations for property testing
validCardInputsGen :: Gen ([ByteString], [ByteString])
validCardInputsGen = do
  num <- elements
    [ "4263982640269299"
    , "4001919257537193"
    , "400000000234324"
    , "371249638761001"
    , "5105105105105100"
    , "6011000990139424"
    ]
  name <- elements
    [ "John Doe"
    , "Jane Mary Smith"
    , "Alice Bob Charlie"
    , "Test User"
    ]
  date <- elements
    [ "12/2026"
    , "06/2029"
    , "01/2025"
    , "11/2030"
    , "12/26"
    ]
  ccv <- elements
    [ "123"
    , "456"
    , "1234"
    , "999"
    ]
  extras <- elements
    [ []
    , ["US"]
    , ["US", "USD"]
    , ["DE", "EUR"]
    ]
  pure ([num, name, date, ccv], extras)

prop_create_permutation_invariant :: Property
prop_create_permutation_invariant = forAll validCardInputsGen $ \(core, extras) ->
  forAll (shuffle (core ++ extras)) $ \permuted ->
    ioProperty $ do
      resCanonical <- execAppEnv $ create (core ++ extras)
      resPermuted  <- execAppEnv $ create permuted
      pure $ resPermuted === resCanonical

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
          case mkCreditCard ["4263982640269299", "John Doe", "12/2026", "123"] of
            Just _ -> assertBool "" True
            Nothing -> assertBool "Should be Just" False
      , testCase "Valid credit card with extra meta" $
          case mkCreditCard ["4263982640269299", "John Doe", "12/2026", "123", "US", "USD"] of
            Just _ -> assertBool "" True
            Nothing -> assertBool "Should be Just" False
      , testCase "Valid credit card in permuted order" $
          case mkCreditCard ["12/2026", "123", "4263982640269299", "John Doe"] of
            Just _ -> assertBool "" True
            Nothing -> assertBool "Should be Just" False
      , testCase "Invalid credit card number" $
          case mkCreditCard ["30569309025904", "John Doe", "12/2026", "123"] of
            Just _ -> assertBool "Should be Nothing" False
            Nothing -> assertBool "" True
      , testCase "Invalid too few fields" $
          case mkCreditCard ["4263982640269299", "John Doe"] of
            Just _ -> assertBool "Should be Nothing" False
            Nothing -> assertBool "" True
      ]
  , testGroup "def"
      [ testCase "default env has correct filepath" $
          binDbFilepath def @?= "data/ranges.csv"
      ]
  , testGroup "initBinDb & setEnv & searchBinDb"
      [ testCase "initBinDb on ranges.csv succeeds" $ do
          mdb <- initBinDb "data/ranges.csv"
          assertBool "initBinDb should succeed" (case mdb of { Just _ -> True; Nothing -> False })
      , testCase "setEnv initializes database" $ do
          env <- setEnv def
          assertBool "binDb should be Just" (case binDb env of { Just _ -> True; Nothing -> False })
      , testCase "searchBinDb finds AmEx" $ do
          res <- execAppEnv (searchBinDb "371240")
          case res of
            Just meta -> do
              schema meta @?= AmEx
              cardType meta @?= Credit
            Nothing -> assertBool "Should find CardMeta for 371240" False
      ]
  , testGroup "checksumCardNumber"
      [ testCase "Valid checksum" $
          checksumCardNumber ((3 :: Int8) :| [7,4,2,4,5,4,5,5,4,0,0,1,2,6]) @?= True
      , testCase "Invalid checksum" $
          checksumCardNumber ((3 :: Int8) :| [7,4,2,4,5,4,5,5,4,0,0,1,2,7]) @?= False
      ]
  , testGroup "digits"
      [ testCase "Valid digits" $
          digits "123" @?= Just ((1 :: Int8) :| [2, 3])
      , testCase "Invalid digits (no digits at start)" $
          digits "abc" @?= Nothing
      , testCase "Digits stops at non-digit" $
          digits "1a3" @?= Just ((1 :: Int8) :| [])
      ]
  , testGroup "luhnSum"
      [ testCase "luhnSum on [4, 2, 6, 3]" $
          luhnSum ([4, 2, 6, 3] :: [Int8]) @?= 20
      ]
  , testGroup "mbTpl"
      [ testCase "Valid mbTpl" $
          mbTpl "4263982640269299" @?= Just ("426398", "2640269299")
      , testCase "Invalid mbTpl" $
          mbTpl "123" @?= Nothing
      ]
  , testGroup "toNonEmptyTpl"
      [ testCase "Valid toNonEmptyTpl" $
          toNonEmptyTpl ([(4 :: Int8), 2, 6, 3, 9, 8], [(2 :: Int8), 6, 4, 0, 2, 6, 9, 2, 9, 9]) @?= Just ("426398", "2640269299")
      , testCase "Invalid toNonEmptyTpl" $
          toNonEmptyTpl ([(4 :: Int8), 2, 6], [(2 :: Int8), 6]) @?= Nothing
      ]
  , testGroup "Read instances"
      [ testGroup "Read RawCreditCard"
          [ testCase "Read from [String] list" $
              read "[\"4263982640269299\", \"John Doe\", \"12/2026\", \"123\"]" @?= MkRawCreditCard "4263982640269299" "John Doe" "12/2026" "123" []
          , testCase "Read from [String] list with meta" $
              read "[\"4263982640269299\", \"John Doe\", \"12/2026\", \"123\", \"US\", \"USD\"]" @?= MkRawCreditCard "4263982640269299" "John Doe" "12/2026" "123" ["US", "USD"]
          , testCase "Read from 4-tuple" $
              read "(\"4263982640269299\", \"John Doe\", \"12/2026\", \"123\")" @?= MkRawCreditCard "4263982640269299" "John Doe" "12/2026" "123" []
          , testCase "Read from constructor" $
              read "MkRawCreditCard \"4263982640269299\" \"John Doe\" \"12/2026\" \"123\" [\"US\"]" @?= MkRawCreditCard "4263982640269299" "John Doe" "12/2026" "123" ["US"]
          , testCase "Read from record" $
              read "MkRawCreditCard { number = \"4263982640269299\", name = \"John Doe\", validTo = \"12/2026\", ccv = \"123\", meta = [\"US\"] }" @?= MkRawCreditCard "4263982640269299" "John Doe" "12/2026" "123" ["US"]
          ]
      , testGroup "Read CreditCard"
          [ testCase "Read valid from list of strings" $
              case (reads "[\"4263982640269299\", \"John Doe\", \"12/2026\", \"123\"]" :: [(CreditCard, String)]) of
                [(cc, "")] -> cc @?= case mkCreditCard ["4263982640269299", "John Doe", "12/2026", "123"] of
                                        Just expected -> expected
                                        Nothing -> error "mkCreditCard should succeed"
                _ -> assertBool "Should parse single CreditCard" False
          , testCase "Read valid with extra meta" $
              case (reads "[\"4263982640269299\", \"John Doe\", \"12/2026\", \"123\", \"US\"]" :: [(CreditCard, String)]) of
                [(cc, "")] -> cc @?= case mkCreditCard ["4263982640269299", "John Doe", "12/2026", "123"] of
                                        Just expected -> expected
                                        Nothing -> error "mkCreditCard should succeed"
                _ -> assertBool "Should parse single CreditCard" False
          , testCase "Read invalid card number fails" $
              case (reads "[\"30569309025904\", \"John Doe\", \"12/2026\", \"123\"]" :: [(CreditCard, String)]) of
                [] -> assertBool "" True
                _  -> assertBool "Should fail parsing invalid card number" False
          , testCase "Read invalid date fails" $
              case (reads "[\"4263982640269299\", \"John Doe\", \"13/2026\", \"123\"]" :: [(CreditCard, String)]) of
                [] -> assertBool "" True
                _  -> assertBool "Should fail parsing invalid date" False
          , testCase "Read invalid name fails" $
              case (reads "[\"4263982640269299\", \"SingleName\", \"12/2026\", \"123\"]" :: [(CreditCard, String)]) of
                [] -> assertBool "" True
                _  -> assertBool "Should fail parsing single name" False
          , testCase "Read from record" $
              case (reads "MkCreditCard { number = \"4263982640269299\", name = \"John Doe\", validTo = (12, 2026), ccv = 123, metaData = Nothing }" :: [(CreditCard, String)]) of
                [(cc, "")] -> cc @?= case mkCreditCard ["4263982640269299", "John Doe", "12/2026", "123"] of
                                        Just expected -> expected
                                        Nothing -> error "mkCreditCard should succeed"
                _ -> assertBool "Should parse record CreditCard" False
          ]
      , testGroup "Read CardNumber"
          [ testCase "Read from quoted string" $
              read "\"4263982640269299\"" @?= MkCardNumber "426398" "2640269299"
          , testCase "Read from bare digits" $
              read "4263982640269299" @?= MkCardNumber "426398" "2640269299"
          , testCase "Read from constructor" $
              read "MkCardNumber \"426398\" \"2640269299\"" @?= MkCardNumber "426398" "2640269299"
          , testCase "Read invalid card number fails" $
              case (reads "12345" :: [(CardNumber, String)]) of
                [] -> assertBool "" True
                _  -> assertBool "Should fail parsing invalid CardNumber" False
          ]
      , testGroup "Read CardName"
          [ testCase "Read from quoted string" $
              read "\"John Doe\"" @?= MkCardName "John Doe"
          , testCase "Read from constructor" $
              read "MkCardName \"John Doe\"" @?= MkCardName "John Doe"
          ]
      , testGroup "Read CCV"
          [ testCase "Read from integer" $
              read "123" @?= MkCCV 123
          , testCase "Read from quoted string" $
              read "\"123\"" @?= MkCCV 123
          , testCase "Read from constructor" $
              read "MkCCV 123" @?= MkCCV 123
          , testCase "Read invalid fails" $
              case (reads "abc" :: [(CCV, String)]) of
                [] -> assertBool "" True
                _  -> assertBool "Should fail parsing non-digits" False
          ]
      , testGroup "Read CardSchema"
          [ testCase "Read Visa" $
              read "Visa" @?= Visa
          , testCase "Read MasterCard" $
              read "MasterCard" @?= MasterCard
          , testCase "Read AmEx" $
              read "AmEx" @?= AmEx
          , testCase "Read Discover" $
              read "Discover" @?= Discover
          ]
      , testGroup "Read CardType"
          [ testCase "Read Debit" $
              read "Debit" @?= Debit
          , testCase "Read Credit" $
              read "Credit" @?= Credit
          ]
      , testGroup "Read CardError"
          [ testCase "Read CardNumberError" $
              read "CardNumberError \"invalid number\"" @?= CardNumberError ("invalid number" :: String)
          , testCase "Read CardValidDateError" $
              read "CardValidDateError \"invalid date\"" @?= CardValidDateError ("invalid date" :: String)
          ]
      ]
  ]
