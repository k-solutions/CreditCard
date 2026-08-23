{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}

module Data.CreditCard.Internal.Errors
  ( CardError (..)
  , parseCardFields
  , parseCardValidations
  ) where

import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Char8           as Ch
import           Data.Char                       (isDigit)
import           Data.CreditCard.Internal.Helpers
import           Data.CreditCard.Internal.Types
import qualified Data.List                       as Lst
import           Data.List.NonEmpty              (NonEmpty (..))
import           Data.Validation

-- | Credit card error responses
data CardError a = CardNumberError a
                 | CardNameError a
                 | CardValidDateError a
                 | CardCCVError a
                 | CardMetaError a
                 deriving (Eq, Read)

instance Monoid a => Enum (CardError a) where
  fromEnum = \case
    CardNumberError _    -> 0
    CardNameError _      -> 1
    CardValidDateError _ -> 2
    CardCCVError _       -> 3
    CardMetaError _      -> 4

  toEnum = \case
    0 -> CardNumberError mempty
    1 -> CardNameError mempty
    2 -> CardValidDateError mempty
    3 -> CardCCVError mempty
    4 -> CardMetaError mempty
    _ -> error "CardError.toEnum: bad argument"

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

-- | Parse fields from [ByteString] in an order-independent manner returning individual Validations.
parseCardValidations :: [ByteString]
                     -> ( Validation (NonEmpty (CardError ByteString)) CardNumber
                        , Validation (NonEmpty (CardError ByteString)) CardName
                        , Validation (NonEmpty (CardError ByteString)) ValidityDate
                        , Validation (NonEmpty (CardError ByteString)) CCV
                        , [ByteString]
                        )
parseCardValidations inputs =
  case inputs of
    (numInp : nameInp : dateInp : ccvInp : extras)
      | Just num <- mkCardNumber numInp
      , Just nm  <- mkCardName nameInp
      , Just vt  <- mkValidDate dateInp
      , Just c   <- mkCCV ccvInp ->
          (Success num, Success nm, Success vt, Success c, extras)
    _ ->
      let (mbDate, rem1) = extractFirst (\x -> Ch.elem '/' x) inputs
          (mbCCV,  rem2) = extractFirst (\x -> BS.length x `elem` [3, 4] && Ch.all isDigit x) rem1
          (mbNum,  rem3) = extractFirst (\x -> BS.length x >= 12 && Ch.all isDigit x) rem2
          (mbName, rem4) = extractFirst (\x -> Ch.elem ' ' x && not (Ch.all isDigit x)) rem3

          vDate = case mbDate of
            Just d  -> validate (CardValidDateError d) mkValidDate d
            Nothing -> Failure (CardValidDateError "" :| [])

          vCCV = case mbCCV of
            Just c  -> validate (CardCCVError c) mkCCV c
            Nothing -> Failure (CardCCVError "" :| [])

          vNum = case mbNum of
            Just n  -> validate (CardNumberError n) mkCardNumber n
            Nothing -> case rem4 of
              (firstRem:_) | Ch.all isDigit firstRem -> validate (CardNumberError firstRem) mkCardNumber firstRem
              _ -> Failure (CardNumberError "" :| [])

          vName = case mbName of
            Just nm -> validate (CardNameError nm) mkCardName nm
            Nothing -> case filter (not . BS.null) rem4 of
              (firstNonEmpty:_) -> validate (CardNameError firstNonEmpty) mkCardName firstNonEmpty
              []                -> Failure (CardNameError "" :| [])

      in (vNum, vName, vDate, vCCV, rem4)
  where
    extractFirst :: (a -> Bool) -> [a] -> (Maybe a, [a])
    extractFirst p xs =
      case Lst.break p xs of
        (before, match : after) -> (Just match, before ++ after)
        (before, [])            -> (Nothing, before)

-- | Parse fields from [ByteString] in an order-independent manner.
-- Resolves Date (contains '/'), CCV (3-4 digits), CardNumber (>= 12 digits), and CardName.
parseCardFields :: [ByteString]
                -> Validation (NonEmpty (CardError ByteString)) (CardNumber, CardName, ValidityDate, CCV, [ByteString])
parseCardFields inputs =
  let (vNum, vName, vDate, vCCV, extras) = parseCardValidations inputs
  in (\num nm vt c -> (num, nm, vt, c, extras)) <$> vNum <*> vName <*> vDate <*> vCCV
