{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}

module Data.CreditCard
  ( CreditCard
  , mkCreditCard
  , mkCardName
  , mkValidDate
  , mkCCV
  ) where

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as Ch
import           Data.Char                (digitToInt)
import           Data.CreditCard.Internal (CCV (..), CardMeta, CardName (..),
                                           ValidityDate)
import           Data.Int
import           Data.List.NonEmpty       as NE
-- import           Data.Time.Calendar       (Year)
import           Data.Time.Calendar.Month (Month (..))
import           Data.Word                (Word8)

-- | Data definitions --

data CardNumber = MkCardNumber
                { bin       :: !ByteString
                , accountId :: !ByteString
                }
                deriving (Show, Eq)

data CreditCard = MkCreditCard
                { number   :: !CardNumber
                , name     :: !CardName
                , validTo  :: !ValidityDate
                , ccv      :: !CCV
                , metaData :: !(Maybe CardMeta)
                }
                deriving (Show, Eq)

-- | API funtions

-- | Smart constuctor to CreditCard with input is (CardNumber, CardName,
-- ValidDate, CCV)
mkCreditCard :: (ByteString, ByteString, ByteString, ByteString) -> Maybe CreditCard
mkCreditCard (numberInp, nameInp, dateInp, ccvInp)
  =   MkCreditCard
  <$> mkCardNumber numberInp
  <*> mkCardName nameInp
  <*> mkValidDate dateInp
  <*> mkCCV ccvInp
  <*> Nothing

-- | CardName constructor
mkCardName :: ByteString -> Maybe CardName
mkCardName nameInp
  = case Ch.split ' ' nameInp of
      [firstName, midName, lastName] -> Just $ MkCardName $ mconcat [firstName, " ", midName, " ", lastName]
      [fstName, lstName]         -> Just $ MkCardName $ mconcat [fstName, " ", lstName]
      _                  -> Nothing

-- | Card CCV  smart constructor
mkCCV :: ByteString -> Maybe CCV
mkCCV inp = MkCCV . fst <$> Ch.readInt inp

-- | Card Valid Date smart constructor
mkValidDate :: ByteString -> Maybe ValidityDate
mkValidDate dateInp =
    case BS.split dateSpliter dateInp of
      [m, y] -> parseDate (m, y)
      _      -> Nothing
  where
    dateSpliter :: Word8
    dateSpliter = aSpliter '/'

    parseInt :: ByteString -> Maybe Integer
    parseInt inp = fst <$> Ch.readInteger inp

    parseDate :: (ByteString, ByteString) -> Maybe ValidityDate
    parseDate (monInp, yearInp) =
      case (parseInt monInp, parseInt yearInp) of
        (Just mon, Just year) | mon >= 1 && mon <= 12 && year >= 2000 -> Just (MkMonth mon, year)
        _                                                             -> Nothing

-- | CardNumber smart constructor
mkCardNumber :: ByteString -> Maybe CardNumber
mkCardNumber cn = uncurry MkCardNumber <$> mbTpl
  where
    toBSTpl :: (NonEmpty Int8, NonEmpty Int8) -> (ByteString, ByteString)
    toBSTpl (neF, neS) = (toBS neF, toBS neS)
    toBS :: NonEmpty Int8 -> ByteString
    toBS = Ch.pack . foldr (\i -> (show i <>)) ""
    mbCardNmb = digits cn
    mbTpl :: Maybe (ByteString, ByteString)
    mbTpl = case checksumCardNumber <$> mbCardNmb of
              Just True -> mbCardNmb >>= toNonEmptyTpl . NE.splitAt 6
              _         -> Nothing
    toNonEmptyTpl :: ([Int8], [Int8]) -> Maybe (ByteString, ByteString)
    toNonEmptyTpl (x, y)
      = case (NE.nonEmpty x, NE.nonEmpty y) of
          (Just neX, Just neY) -> toBSTpl <$> checkElemCardNumber (neX, neY)
          _                    -> Nothing

-- | Helpers --
--
aSpliter :: Char -> Word8
aSpliter = fromIntegral . fromEnum

digits :: ByteString -> Maybe (NonEmpty Int8)
digits = Just . NE.unfoldr go
  where
    go :: ByteString -> (Int8, Maybe ByteString)
    go inp =
      case Ch.uncons inp  of
        Nothing       -> (0, Nothing)
        Just (ch, ts) -> (digitToInt8 ch, Just ts)

digitToInt8 :: Char -> Int8
digitToInt8 = fromIntegral . digitToInt

-- | Checks Card Number BIN/IIN and account id lengths
checkElemCardNumber :: (NonEmpty Int8, NonEmpty Int8) -> Maybe (NonEmpty Int8, NonEmpty Int8)
checkElemCardNumber (cBin, accId)
  | NE.length cBin == 6 && NE.length accId >= 10 = Just (cBin, accId)
  | otherwise = Nothing

-- | Checks CardNumber with Luhn algorithm
checksumCardNumber :: NonEmpty Int8 -> Bool
checksumCardNumber nmb = NE.last nmb == (10 - (snd checkSum `mod` 10))
  where
    checkSum = foldr getSum (0,0) $ NE.init nmb
    getSumNum i
      | i > 10 = 1 + (i `mod` 10)
      | otherwise = i
    getSum it (ix, acc)
      | even ix = (ix + 1, getSumNum ( 2 * it) + acc)
      | otherwise = (ix + 1, it + acc)

