{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}

module Data.CreditCard.Internal.Parser
  ( TokenResult (..)
  , classifyToken
  , tryParseNum
  , tryParseDate
  , tryParseCCV
  , tryParseName
  , parseCardValidations
  , parseCardValidation
  , parseCardFields
  ) where

import           Control.Applicative              ((<|>))
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as Ch
import           Data.Char                        (isDigit)
import           Data.CreditCard.Internal.Errors
import           Data.CreditCard.Internal.Helpers
import           Data.CreditCard.Internal.Types
import           Data.Foldable                    (foldl')
import           Data.List.NonEmpty               (NonEmpty (..))
import           Data.Maybe                       (fromMaybe)
import           Data.Validation                  (Validation (..))

-- | Intermediate result for a single token
data TokenResult
  = ParsedNum CardNumber
  | ParsedName CardName
  | ParsedDate ValidityDate
  | ParsedCCV CCV
  | FailedWith (CardError ByteString)
  | Unparsed ByteString
  deriving (Show, Eq)

-- | Classify a single ByteString token into TokenResult
classifyToken :: ByteString -> TokenResult
classifyToken bs = fromMaybe (Unparsed bs) $
      (either FailedWith ParsedNum  <$> tryParseNum bs)
  <|> (either FailedWith ParsedDate <$> tryParseDate bs)
  <|> (either FailedWith ParsedCCV  <$> tryParseCCV bs)
  <|> (either FailedWith ParsedName <$> tryParseName bs)

-- | Try to parse token as CardNumber
tryParseNum :: ByteString -> Maybe (Either (CardError ByteString) CardNumber)
tryParseNum bs
  | not (BS.null bs) && Ch.all isDigit bs && BS.length bs >= 12 =
      case mkCardNumber bs of
        Just cn -> Just (Right cn)
        Nothing -> Just (Left (CardNumberError bs))
  | otherwise = Nothing

-- | Try to parse token as ValidityDate
tryParseDate :: ByteString -> Maybe (Either (CardError ByteString) ValidityDate)
tryParseDate bs
  | isDateCandidate bs =
      case mkValidDate bs of
        Just vd -> Just (Right vd)
        Nothing -> Just (Left (CardValidDateError bs))
  | otherwise = Nothing
  where
    isDateCandidate s =
      case Ch.split '/' s of
        [m, y] -> not (BS.null m) && not (BS.null y) && Ch.all isDigit m && Ch.all isDigit y
        _      -> False

-- | Try to parse token as CCV
tryParseCCV :: ByteString -> Maybe (Either (CardError ByteString) CCV)
tryParseCCV bs
  | not (BS.null bs) && Ch.all isDigit bs && BS.length bs `elem` [3, 4] =
      case mkCCV bs of
        Just c  -> Just (Right c)
        Nothing -> Just (Left (CardCCVError bs))
  | otherwise = Nothing

-- | Try to parse token as CardName
tryParseName :: ByteString -> Maybe (Either (CardError ByteString) CardName)
tryParseName bs
  | Ch.elem ' ' bs && not (Ch.all isDigit bs) =
      case mkCardName bs of
        Just nm -> Just (Right nm)
        Nothing -> Just (Left (CardNameError bs))
  | otherwise = Nothing

-- | Parse fields from [ByteString] in an order-independent manner returning individual Validations.
parseCardValidations :: [ByteString]
                     -> (Validations CreditCard, [ByteString])
parseCardValidations inputs =
  let
    (mNumErr, mNameErr, mDateErr, mCCVErr, unparsed, mNum, mName, mDate, mCCV) =
      foldl' processToken (Nothing, Nothing, Nothing, Nothing, [], Nothing, Nothing, Nothing, Nothing) inputs

    require defErr mbVal mbErr = case mbVal of
      Just val -> Success val
      Nothing  -> case mbErr of
        Just e  -> Failure (e :| [])
        Nothing -> Failure (defErr :| [])

    vNum  = require (CardNumberError "")    mNum  mNumErr
    vName = require (CardNameError "")      mName mNameErr
    vDate = require (CardValidDateError "") mDate mDateErr
    vCCV  = require (CardCCVError "")       mCCV  mCCVErr
    vCard = MkCreditCard <$> vNum <*> vName <*> vDate <*> vCCV <*> pure Nothing
  in (vCard, reverse unparsed)
  where
    processToken (numErr, nameErr, dateErr, ccvErr, u, num, name, date, ccv) bs =
      case classifyToken bs of
        ParsedNum  n                      -> (numErr, nameErr, dateErr, ccvErr, u, num <|> Just n, name, date, ccv)
        ParsedName n                      -> (numErr, nameErr, dateErr, ccvErr, u, num, name <|> Just n, date, ccv)
        ParsedDate d                      -> (numErr, nameErr, dateErr, ccvErr, u, num, name, date <|> Just d, ccv)
        ParsedCCV  c                      -> (numErr, nameErr, dateErr, ccvErr, u, num, name, date, ccv <|> Just c)
        FailedWith (CardNumberError e)    -> (numErr <|> Just (CardNumberError e), nameErr, dateErr, ccvErr, u, num, name, date, ccv)
        FailedWith (CardNameError e)      -> (numErr, nameErr <|> Just (CardNameError e), dateErr, ccvErr, u, num, name, date, ccv)
        FailedWith (CardValidDateError e) -> (numErr, nameErr, dateErr <|> Just (CardValidDateError e), ccvErr, u, num, name, date, ccv)
        FailedWith (CardCCVError e)       -> (numErr, nameErr, dateErr, ccvErr <|> Just (CardCCVError e), u, num, name, date, ccv)
        FailedWith (CardMetaError _)      -> (numErr, nameErr, dateErr, ccvErr, u, num, name, date, ccv)
        Unparsed   bs'                    -> (numErr, nameErr, dateErr, ccvErr, bs' : u, num, name, date, ccv)

-- | Alias for parseCardValidations
parseCardValidation :: [ByteString]
                    -> (Validations CreditCard, [ByteString])
parseCardValidation = parseCardValidations

-- | Parse fields from [ByteString] in an order-independent manner.
parseCardFields :: [ByteString]
                -> Validation WithError (CardNumber, CardName, ValidityDate, CCV, [ByteString])
parseCardFields inputs =
  let (vCard, extras) = parseCardValidations inputs
  in (\card -> (card.number, card.name, card.validTo, card.ccv, extras)) <$> vCard
