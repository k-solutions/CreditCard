{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}

module Data.CreditCard.Internal.Types
  ( CardSchema (..)
  , CardType (..)
  , CardBrand (..)
  , CardBank (..)
  , CardMeta (..)
  , BinData (..)
  , BinDb
  , Env (..)
  , CCV (..)
  , ValidityDate
  , CardName (..)
  , CardNumber (..)
  , RawCreditCard (..)
  , CreditCard (..)
  ) where

import           Country               (Country)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as Ch
import           Data.Csv              (FromField (..), FromNamedRecord (..))
import qualified Data.Csv              as CSV
import           Data.Default          (Default (..))
import           Data.IORef            (IORef)
import           Data.Time.Calendar    (MonthOfYear, Year)
import           Data.Trie             (Trie)
import           GHC.Generics          (Generic)
import           Say                   (sayShow)

-- | Env for the Credit Card wrapping application
data Env = MkEnv
  { logger        :: !(ByteString -> IO ())
  , logFilepath   :: !(Maybe FilePath)
  , binDbFilepath :: !FilePath
  , binDb         :: !(Maybe BinDb)
  }
  deriving (Generic)

instance Default Env where
  def = MkEnv
    { logger        = sayShow
    , logFilepath   = Nothing
    , binDbFilepath = "data/ranges.csv"
    , binDb         = Nothing
    }

-- | Card CCV representation (masked in Show for privacy)
newtype CCV = MkCCV Int
  deriving (Eq, Read, Generic)

instance Show CCV where
  show _ = "***"

type ValidityDate = (MonthOfYear, Year)

data CardSchema = AmEx
                | Bankcard
                | BmoAbmCard
                | CanadianImperialBank
                | ChinaTUnion
                | Dankort
                | ChinaUnionPaid
                | DinersCarteBlanche
                | DinersEnRoute
                | DinersInt
                | DinersNorthAm
                | Discover
                | InterPayment
                | InstaPayment
                | HSBCBankCanadaCard
                | JCB
                | LankaPay
                | Laser
                | MaestroUK
                | Maestro
                | MasterCard
                | MIR
                | NPSPridnestrovie
                | RoyalBankCanadaCard
                | RuPay
                | ScotiaBankCard
                | Solo
                | Switch
                | TDCanadaTrustAccessCard
                | Troy
                | Visa
                | Verve
                | UATP
                | UkrCard
                deriving (Show, Eq, Read, Generic)

instance FromField CardSchema where
  parseField = pure . fromCardSchemaField

fromCardSchemaField :: ByteString -> CardSchema
fromCardSchemaField = \case
  "amex"       -> AmEx
  "diners"     -> DinersInt
  "visa"       -> Visa
  "mastercard" -> MasterCard
  "discover"   -> Discover
  "unionpay"   -> ChinaUnionPaid
  _            -> Bankcard

newtype CardName = MkCardName ByteString
  deriving (Show, Eq, Read, Generic)

newtype CardBrand = MkCardBrand ByteString
  deriving (Show, Eq, Read, Generic)

data CardType = Debit
              | Credit
              deriving (Show, Eq, Read, Generic)

instance FromField CardType where
  parseField = pure . fromCardTypeField

fromCardTypeField :: ByteString -> CardType
fromCardTypeField = \case
  "credit" -> Credit
  _        -> Debit

data CardBank = MkCardBank
  { name  :: !ByteString
  , url   :: !(Maybe ByteString)
  , phone :: !(Maybe ByteString)
  , city  :: !(Maybe ByteString)
  }
  deriving (Show, Eq, Read, Generic)

data CardMeta = MkCardMeta
  { schema        :: !CardSchema
  , cardType      :: !CardType
  , cardBrand     :: !(Maybe CardBrand)
  , issuerCountry :: !(Maybe Country)
  , issuer        :: !(Maybe CardBank)
  }
  deriving (Show, Eq, Generic)

data BinData = MkBinData
  { iin         :: !Int
  , iinRangeEnd :: !(Maybe Int)
  , scheme      :: !CardSchema
  , cardType    :: !CardType
  }
  deriving (Show, Eq, Generic)

instance FromNamedRecord BinData where
  parseNamedRecord m = MkBinData
    <$> m CSV..: "iin_start"
    <*> m CSV..: "iin_end"
    <*> m CSV..: "scheme"
    <*> m CSV..: "type"

type BinDb = IORef (Trie CardMeta)

-- | CardNumber holds credit card number as record of 2 fields with BIN/IIN and Account ID number
data CardNumber = MkCardNumber
  { bin       :: !ByteString
  , accountId :: !ByteString
  }
  deriving (Eq, Read, Generic)

instance Show CardNumber where
  show cardNmb = show $ cardNmb.bin <> Ch.map (const '*') masked <> notMasked
    where (masked, notMasked) = BS.splitAt (BS.length cardNmb.accountId - 4) cardNmb.accountId

-- | Raw credit card input data before validation, with optional extra metadata
data RawCreditCard = MkRawCreditCard
  { number  :: !ByteString
  , name    :: !ByteString
  , validTo :: !ByteString
  , ccv     :: !ByteString
  , meta    :: ![ByteString]
  }
  deriving (Show, Eq, Read, Generic)

-- | Full credit card info type with metadata information
data CreditCard = MkCreditCard
  { number   :: !CardNumber
  , name     :: !CardName
  , validTo  :: !ValidityDate
  , ccv      :: !CCV
  , metaData :: !(Maybe CardMeta)
  }
  deriving (Show, Eq, Generic)
