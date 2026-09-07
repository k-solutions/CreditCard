{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}

{-|
Module      : Data.CreditCard
Description : Credit card validation, order-independent parsing, and BIN lookup
Copyright   : (c) Hristo Kochev, 2026
License     : BSD-3-Clause
Maintainer  : h.l.kochev@gmail.com
Stability   : experimental

Main entry point for the CreditCard library. Provides domain models for credit cards,
smart constructors, order-independent field extraction from token lists, masked @Show@
representations, and BIN/IIN range database integration.
-}
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
  , parseCardValidations
  , parseCardValidation
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
  let (vCard, _extras) = parseCardValidations inputs
  mapM (\card -> do
    mbMeta <- searchBinDb card.number.bin
    pure card { metaData = mbMeta }
    ) vCard

-- | Backward-compatible alias for create
createCreditCard :: (MonadIO m, MonadReader Env m)
                 => [ByteString]
                 -> m (Validation (NonEmpty (CardError ByteString)) CreditCard)
createCreditCard = create
