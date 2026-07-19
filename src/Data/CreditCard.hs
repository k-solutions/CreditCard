{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData,LambdaCase #-}

module Data.CreditCard
  ( CreditCard
  , CardMeta (..)
  , Env (..)
  , def
  , mkCreditCard
  , mkCardName
  , mkValidDate
  , mkCCV
  , mkCardNumber
  , createCreditCard
  , searchBinDb
  , setEnv
  , checksumCardNumber  -- ^ temporay enabled for testing only 
  , digits
  , initBinDb 
  , luhnSum
  , mbTpl
  , toNonEmptyTpl
  ) where

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as Ch
import           Data.Char                (digitToInt)
import           Data.CreditCard.Internal -- (checksumCardNumber, digits, def, searchBinDb, Env (..), CCV (..), CardMeta, CardName (..),
                                          -- ValidityDate)
import           Data.Int
import           Data.List.NonEmpty       as NE
import           Data.Word                (Word8)
import qualified Data.List as Lst 
import Data.Char8 (isDigit)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Validation

-- | Data definitions --

-- | Credit card error responses
data CardError a = CardNumberError a
                 | CardNameError a
                 | CardValidDateError a
                 | CardCCVError a
                 | CardMetaError a 
--              deriving (Enum)
instance Monoid a => Enum (CardError a) where 
    fromEnum = \case
      CardNumberError _     -> 0 
      CardNameError _       -> 1
      CardValidDateError _  -> 2
      CardCCVError _        -> 3 
      CardMetaError _       -> 4  

    toEnum = \case
      0 -> CardNumberError mempty
      1 -> CardNameError mempty
      2 -> CardValidDateError mempty 
      3 -> CardCCVError mempty 
      4 -> CardMetaError mempty

instance Monoid a => Semigroup (CardError a) where
    (<>) e1 e2 
        | fromEnum e1 > fromEnum e2 = e1
        | otherwise = e2 

instance Show a => Show (CardError a) where
   show = \case 
    CardNumberError v    -> "Wrong value in card number: " <> show v 
    CardNameError   v    -> "Wrong value for card name: " <> show v
    CardValidDateError v -> "Wrong value for valid date: " <> show v
    CardCCVError v       -> "Wrong CCV value: " <> show v 
    CardMetaError v      -> "Wrong meta info!" <> show v

-- | CardNumber holds credit card number as record of 2 fields with BIN/IIN
-- and Account ID number   
data CardNumber = MkCardNumber
                { bin       :: !ByteString
                , accountId :: !ByteString
                }
                deriving (Eq)

instance Show CardNumber where
  show cardNmb = show $ cardNmb.bin <> Ch.map (const '*') masked <> notMasked    
    where (masked, notMasked) = BS.splitAt (BS.length cardNmb.accountId - 4) cardNmb.accountId 

-- | Full credit card info type with metadata information
data CreditCard = MkCreditCard
                { number   :: !CardNumber
                , name     :: !CardName
                , validTo  :: !ValidityDate
                , ccv      :: !CCV
                , metaData :: !(Maybe CardMeta)
                }
                deriving (Show, Eq)

-- | API funtions

-- | A valid CreditCard with Meta data with posible error reporting
--  (cardNumber, cardName, cardDate, cardCCV)
-- Examples: 
-- >>> createCreditCard ("400000000234324", "Test name", "12/26", "123")
-- undefined 
createCreditCard :: (MonadIO m, MonadReader Env m) 
                 => (ByteString, ByteString, ByteString, ByteString) 
                 -> m (Validation (CardError ByteString) CreditCard)
createCreditCard (numberInp, nameInp, dateInp, ccvInp) = do
  let vCardNmb   = validate (CardNumberError numberInp) mkCardNumber numberInp
      vCardName  = validate (CardNameError nameInp) mkCardName nameInp
      vValidDate = validate (CardValidDateError dateInp) mkValidDate dateInp
      vCCV       = validate (CardCCVError ccvInp) mkCCV ccvInp
      -- vCEmptyMeta = validate CardMetaError (const Nothing) Nothing  
      --vCC        = MkCreditCard <$> vCardNmb <*> vCardName <*> vValidDate <*> vCCV <*> vCEmptyMeta 
  vCardMeta <- mapM (\cardNmb -> searchBinDb cardNmb.bin) vCardNmb
  pure $ MkCreditCard <$> vCardNmb <*> vCardName <*> vValidDate <*> vCCV <*> vCardMeta  

-- | Smart constuctor to CreditCard with input is (CardNumber, CardName,
-- ValidDate, CCV)
--
-- Examples:
--
-- >>> mkCreditCard ("30569309025904", "TestFirs TestLast", "06/29", "123")
-- Nothing
--
mkCreditCard :: (ByteString, ByteString, ByteString, ByteString) -> Maybe CreditCard
mkCreditCard (numberInp, nameInp, dateInp, ccvInp)
  =   MkCreditCard
  <$> mkCardNumber numberInp
  <*> mkCardName nameInp
  <*> mkValidDate dateInp
  <*> mkCCV ccvInp
  <*> Nothing

-- | CardName constructor
--
-- Examples:
--
-- >>> mkCardName "First Last"
-- Just (MkCardName "First  Last")
mkCardName :: ByteString -> Maybe CardName
mkCardName nameInp
  = case Ch.split ' ' nameInp of
      [fstName, midName, lstName] | fstName /= "" && lstName /= "" -> Just $ MkCardName $ mconcat [fstName, " ", midName, " ", lstName]
      [fstName, lstName] | fstName /= "" && lstName /= ""  -> Just $ MkCardName $ mconcat [fstName, " ", lstName]
      _                  -> Nothing

-- | Card CCV  smart constructor
--
-- Examples:
--
-- >>> mkCCV "123"
-- Just MkCCV "123"
mkCCV :: ByteString -> Maybe CCV
mkCCV inp = MkCCV . fst <$> Ch.readInt inp

-- | Card Valid Date smart constructor
--
-- Examples:
--
-- >>>
-- mkValidDate "06/29"
-- Just (6, 29)
mkValidDate :: ByteString -> Maybe ValidityDate
mkValidDate dateInp =
    case Ch.split '/' dateInp of
      [m, y] -> parseDate (m, y)
      _      -> Nothing
  where
    parseInt :: ByteString -> Maybe Integer
    parseInt inp = fst <$> Ch.readInteger inp

    validYear year = any ($ year) [\y -> y >=1000 && y >= 2020,\y -> y <= 100 && y <= 20] 
    parseDate :: (ByteString, ByteString) -> Maybe ValidityDate
    parseDate (monInp, yearInp) =
      case (parseInt monInp, parseInt yearInp) of
        (Just mon, Just year) | mon >= 1 && mon <= 12 && validYear year -> Just (fromIntegral mon, year)
        _ -> Nothing

-- | CardNumber smart constructor
--
-- Examples:
--
-- >>> mkCardNumber ("30569309025904"  :: ByteSyting )
-- Just MkCardNumber { bin: "305693", accountId: "09025904" }
mkCardNumber :: ByteString -> Maybe CardNumber
mkCardNumber cn = uncurry MkCardNumber <$> mbTpl cn

toBSTpl :: (NonEmpty Int8, NonEmpty Int8) -> (ByteString, ByteString)
toBSTpl (neF, neS) = (toBS neF, toBS neS)

toBS :: NonEmpty Int8 -> ByteString
toBS = Ch.pack . foldr (\i -> (show i <>)) ""

mbTpl :: ByteString -> Maybe (ByteString, ByteString)
mbTpl cn = mbCardNmb >>= toNonEmptyTpl . NE.splitAt 6
  where mbCardNmb = digits cn
 
toNonEmptyTpl :: ([Int8], [Int8]) -> Maybe (ByteString, ByteString)
toNonEmptyTpl (x, y)
      = case (NE.nonEmpty x, NE.nonEmpty y) of
          (Just neX, Just neY) -> toBSTpl <$> checkElemCardNumber (neX, neY)
          _                    -> Nothing

-- | Helpers --

digits :: ByteString -> Maybe (NonEmpty Int8)
digits = NE.nonEmpty . Lst.unfoldr go 
  where
    go :: ByteString -> Maybe (Int8, ByteString)
    go inp =
      case Ch.uncons inp of
        Nothing       -> Nothing
        Just (ch, ts) ->  (, ts) <$> digitToInt8 ch

digitToInt8 :: Char -> Maybe Int8
digitToInt8 ch 
  | isDigit ch = Just . fromIntegral $ digitToInt ch
  | otherwise  = Nothing 

-- | Checks Card Number BIN/IIN and account id lengths
checkElemCardNumber :: (NonEmpty Int8, NonEmpty Int8) -> Maybe (NonEmpty Int8, NonEmpty Int8)
checkElemCardNumber (cBin, accId)
  | NE.length cBin == 6 && NE.length accId > 8 = Just (cBin, accId)
  | otherwise = Nothing

luhnDbl :: Int8 -> Int8
luhnDbl n 
  | dbl > 9 = dbl - 9 
  | otherwise = dbl
 where dbl = n * 2 

-- | Checks CardNumber with Luhn algorithm
checksumCardNumber :: NonEmpty Int8 -> Bool
checksumCardNumber nmb = go == fromIntegral (NE.last nmb)
  where go = (10 - (luhnSum (NE.init nmb) `mod` 10)) `mod` 10 

luhnSum :: [Int8] -> Int8 
luhnSum ns = sum $ Prelude.zipWith ($) (Prelude.cycle [id, luhnDbl]) ns
