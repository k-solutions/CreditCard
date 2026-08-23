{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}

module Data.CreditCard
  ( CreditCard (..)
  , RawCreditCard (..)
  , mkRawCreditCard
  , mkCreditCardFromRaw
  , CardNumber (..)
  , CardName (..)
  , CardBrand (..)
  , CardBank (..)
  , CardMeta (..)
  , CardSchema (..)
  , CardType (..)
  , CardError (..)
  , CCV (..)
  , ValidityDate
  , Env (..)
  , def
  , mkCreditCard
  , mkCardName
  , mkValidDate
  , mkCCV
  , mkCardNumber
  , create
  , createCreditCard
  , searchBinDb
  , setEnv
  , checksumCardNumber
  , digits
  , initBinDb
  , luhnSum
  , mbTpl
  , toNonEmptyTpl
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString               (ByteString)
import           Data.CreditCard.Internal
import           Data.Default                  (def)
import           Data.List.NonEmpty            (NonEmpty (..))
import           Data.Validation

-- | A valid CreditCard with Meta data and error reporting.
-- Accepts [ByteString] inputs in any field order (number, name, validTo, ccv, ...extras).
--
-- Examples:
--
-- >>> create ["400000000234324", "Test name", "12/26", "123"]
-- undefined
create :: (MonadIO m, MonadReader Env m)
       => [ByteString]
       -> m (Validation (NonEmpty (CardError ByteString)) CreditCard)
create inputs = do
  let (vCardNmb, vCardName, vValidDate, vCCV, _extras) = parseCardValidations inputs
  vCardMeta <- mapM (\cardNmb -> searchBinDb cardNmb.bin) vCardNmb
  pure $ MkCreditCard <$> vCardNmb <*> vCardName <*> vValidDate <*> vCCV <*> vCardMeta

-- | Backward-compatible alias for create
createCreditCard :: (MonadIO m, MonadReader Env m)
                 => [ByteString]
                 -> m (Validation (NonEmpty (CardError ByteString)) CreditCard)
createCreditCard = create
