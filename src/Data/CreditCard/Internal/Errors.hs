{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}

module Data.CreditCard.Internal.Errors
  ( CardError (..)
  , WithError
  , Validations
  ) where

import           Data.ByteString    (ByteString)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Validation    (Validation (..))
import           GHC.Generics       (Generic)

-- | Credit card error responses
data CardError a = CardNumberError a
                 | CardNameError a
                 | CardValidDateError a
                 | CardCCVError a
                 | CardMetaError a
                 deriving (Eq, Read, Generic)

type WithError = NonEmpty (CardError ByteString)

-- | Validations is type which holds all errors you may encounter
type Validations a = Validation WithError a

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
