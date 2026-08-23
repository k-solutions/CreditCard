{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}

module Data.CreditCard.Internal.Helpers
  ( validate
  , mkCardName
  , mkValidDate
  , mkCCV
  , mkCardNumber
  , mkRawCreditCard
  , mkCreditCard
  , mkCreditCardFromRaw
  , checksumCardNumber
  , digits
  , luhnSum
  , luhnDbl
  , mbTpl
  , toNonEmptyTpl
  , checkElemCardNumber
  , digitToInt8
  , toBSTpl
  , toBS
  ) where

import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as Ch
import           Data.Char                     (digitToInt, isDigit)
import           Data.CreditCard.Internal.Types
import           Data.Int                      (Int8)
import qualified Data.List                     as Lst
import           Data.List.NonEmpty            (NonEmpty (..))
import qualified Data.List.NonEmpty            as NE
import           Data.Validation

-- | Helper to validate a value with a parser function into a Validation
validate :: err -> (a -> Maybe b) -> a -> Validation (NonEmpty err) b
validate err f x = maybe (Failure (err :| [])) Success (f x)

-- | CardName smart constructor
mkCardName :: ByteString -> Maybe CardName
mkCardName nameInp =
  case Ch.split ' ' nameInp of
    [fstName, midName, lstName]
      | fstName /= "" && lstName /= "" && notAllDigits fstName && notAllDigits lstName ->
          Just $ MkCardName $ mconcat [fstName, " ", midName, " ", lstName]
    [fstName, lstName]
      | fstName /= "" && lstName /= "" && notAllDigits fstName && notAllDigits lstName ->
          Just $ MkCardName $ mconcat [fstName, " ", lstName]
    _ -> Nothing
  where
    notAllDigits bs = not (BS.null bs) && not (Ch.all isDigit bs)

-- | Card CCV smart constructor
mkCCV :: ByteString -> Maybe CCV
mkCCV inp =
  case Ch.readInt inp of
    Just (n, rest) | BS.null rest && n >= 0 && BS.length inp `elem` [3, 4] -> Just (MkCCV n)
    _ -> Nothing

-- | Card Valid Date smart constructor
mkValidDate :: ByteString -> Maybe ValidityDate
mkValidDate dateInp =
  case Ch.split '/' dateInp of
    [m, y] -> parseDate (m, y)
    _      -> Nothing
  where
    parseInt :: ByteString -> Maybe Integer
    parseInt inp = case Ch.readInteger inp of
      Just (n, rest) | BS.null rest -> Just n
      _                             -> Nothing

    validYear year = any ($ year) [\y -> y >= 1000 && y >= 2020, \y -> y >= 20 && y <= 99]
    parseDate :: (ByteString, ByteString) -> Maybe ValidityDate
    parseDate (monInp, yearInp) =
      case (parseInt monInp, parseInt yearInp) of
        (Just mon, Just year) | mon >= 1 && mon <= 12 && validYear year -> Just (fromIntegral mon, year)
        _ -> Nothing

-- | CardNumber smart constructor
mkCardNumber :: ByteString -> Maybe CardNumber
mkCardNumber cn = uncurry MkCardNumber <$> mbTpl cn

-- | Helper to create RawCreditCard from list of ByteStrings: [number, name, validTo, ccv, ...]
mkRawCreditCard :: [ByteString] -> Maybe RawCreditCard
mkRawCreditCard = \case
  (num : nm : vt : c : extra) -> Just $ MkRawCreditCard num nm vt c extra
  _                           -> Nothing

-- | Smart constructor for CreditCard from [ByteString]
mkCreditCard :: [ByteString] -> Maybe CreditCard
mkCreditCard inputs =
  case inputs of
    (numInp : nameInp : dateInp : ccvInp : _extras)
      | Just num <- mkCardNumber numInp
      , Just nm  <- mkCardName nameInp
      , Just vt  <- mkValidDate dateInp
      , Just c   <- mkCCV ccvInp ->
          Just $ MkCreditCard num nm vt c Nothing
    _ ->
      let (mbDate, rem1) = extractFirst (\x -> Ch.elem '/' x) inputs
          (mbCCV,  rem2) = extractFirst (\x -> BS.length x `elem` [3, 4] && Ch.all isDigit x) rem1
          (mbNum,  rem3) = extractFirst (\x -> BS.length x >= 12 && Ch.all isDigit x) rem2
          (mbName, _)    = extractFirst (\x -> Ch.elem ' ' x && not (Ch.all isDigit x)) rem3
      in MkCreditCard
           <$> (mbNum >>= mkCardNumber)
           <*> (mbName >>= mkCardName)
           <*> (mbDate >>= mkValidDate)
           <*> (mbCCV >>= mkCCV)
           <*> Just Nothing
  where
    extractFirst :: (a -> Bool) -> [a] -> (Maybe a, [a])
    extractFirst p xs =
      case Lst.break p xs of
        (before, match : after) -> (Just match, before ++ after)
        (before, [])            -> (Nothing, before)

-- | Smart constructor for CreditCard from RawCreditCard
mkCreditCardFromRaw :: RawCreditCard -> Maybe CreditCard
mkCreditCardFromRaw (MkRawCreditCard num nm vt c _meta) =
  MkCreditCard
    <$> mkCardNumber num
    <*> mkCardName nm
    <*> mkValidDate vt
    <*> mkCCV c
    <*> Just Nothing

-- | Helper functions for CardNumber & Luhn algorithm --

toBSTpl :: (NonEmpty Int8, NonEmpty Int8) -> (ByteString, ByteString)
toBSTpl (neF, neS) = (toBS neF, toBS neS)

toBS :: NonEmpty Int8 -> ByteString
toBS = Ch.pack . foldr (\i -> (show i <>)) ""

mbTpl :: ByteString -> Maybe (ByteString, ByteString)
mbTpl cn = case digits cn of
  Just dns | checksumCardNumber dns -> toNonEmptyTpl (NE.splitAt 6 dns)
  _ -> Nothing

toNonEmptyTpl :: ([Int8], [Int8]) -> Maybe (ByteString, ByteString)
toNonEmptyTpl (x, y) =
  case (NE.nonEmpty x, NE.nonEmpty y) of
    (Just neX, Just neY) -> toBSTpl <$> checkElemCardNumber (neX, neY)
    _                    -> Nothing

digits :: ByteString -> Maybe (NonEmpty Int8)
digits = NE.nonEmpty . Lst.unfoldr go
  where
    go :: ByteString -> Maybe (Int8, ByteString)
    go inp =
      case Ch.uncons inp of
        Nothing       -> Nothing
        Just (ch, ts) -> (, ts) <$> digitToInt8 ch

digitToInt8 :: Char -> Maybe Int8
digitToInt8 ch
  | isDigit ch = Just . fromIntegral $ digitToInt ch
  | otherwise  = Nothing

checkElemCardNumber :: (NonEmpty Int8, NonEmpty Int8) -> Maybe (NonEmpty Int8, NonEmpty Int8)
checkElemCardNumber (cBin, accId)
  | NE.length cBin == 6 && NE.length accId > 8 = Just (cBin, accId)
  | otherwise = Nothing

luhnDbl :: Int8 -> Int8
luhnDbl n
  | dbl > 9   = dbl - 9
  | otherwise = dbl
  where dbl = n * 2

checksumCardNumber :: NonEmpty Int8 -> Bool
checksumCardNumber nmb = go == fromIntegral (NE.last nmb)
  where go = (10 - (luhnSum (NE.init nmb) `mod` 10)) `mod` 10

luhnSum :: [Int8] -> Int8
luhnSum ns = sum $ Prelude.zipWith ($) (Prelude.cycle [luhnDbl, id]) (Prelude.reverse ns)
