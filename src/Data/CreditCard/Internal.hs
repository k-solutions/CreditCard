{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Data.CreditCard.Internal
( CardName(..)
, CardType(..)
, CardMeta(..)
, CardSchema(..)
, CCV(..)
, Env (..)
, ValidityDate
, setEnv
, initBinDb
, searchBinDb
, def
) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Country                  (Country)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as Ch
import qualified Data.ByteString.Lazy     as BL
import           Data.Csv                 (FromField, FromNamedRecord)
import qualified Data.Csv                 as CSV
import           Data.Default
import           Data.IORef
import           Data.Time.Calendar       (Year, MonthOfYear)
import           Data.Trie                (Trie)
import qualified Data.Trie                as Trie
import           Data.Vector              (Vector)
import qualified Data.Vector              as V
import           GHC.Generics
import           Say
import           System.IO.Memoize        (eagerlyOnce)

-- | Data types

data Env = MkEnv                                              -- ^ Env for the Credit Card wrapping application
         { logger        :: !(ByteString -> IO ())       -- ^ Env log function
         , logFilepath   :: !(Maybe FilePath)            -- ^ logfile filepath
         , binDbFilepath :: !FilePath                    -- ^ Bin / IIN numbers CSV filepath
         , binDb         :: !(Maybe BinDb)               -- ^ IORef into in memory BinDb
         }
         deriving (Generic)

instance Default Env where 
    def = MkEnv { logger = sayShow
                , logFilepath = Nothing
                , binDbFilepath = "data/ranges.csv"
                , binDb = Nothing
                }

-- | Card Data type definitions --

newtype CCV = MkCCV Int deriving Eq

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
                deriving (Show, Eq)

instance FromField CardSchema where
   parseField = pure . fromCardSchemaField

newtype CardName = MkCardName ByteString deriving (Show, Eq)
newtype CardBrand = MkCardBrand ByteString deriving (Show, Eq)

data CardType = Debit
              | Credit
              deriving (Show, Eq)

instance FromField CardType where
   parseField = pure . fromCardTypeField

data CardBank = MkCardBank
              { name  :: !ByteString
              , url   :: !(Maybe ByteString)
              , phone :: !(Maybe ByteString)
              , city  :: !(Maybe ByteString)
              }
              deriving (Show, Eq)

data CardMeta = MkCardMeta
              { schema        :: !CardSchema
              , cardType      :: !CardType
              , cardBrand     :: !(Maybe CardBrand)
              , issuerCountry :: !(Maybe Country)
              , issuer        :: !(Maybe CardBank)
              }
              deriving (Show, Eq)

data BinData = MkBinData
             { iin         :: !Int
             , iinRangeEnd :: !(Maybe Int)
             , scheme      :: !CardSchema
             , cardType    :: !CardType
             } deriving (Show, Eq, Generic)

instance FromNamedRecord BinData where
    parseNamedRecord m =    MkBinData
                       <$>  m CSV..: "iin_start"
                       <*>  m CSV..: "iin_end"
                       <*>  m CSV..: "scheme"
                       <*>  m CSV..: "type"

type BinDb = IORef (Trie CardMeta)

-- | A default environment 
setEnv :: MonadIO m => Env -> m Env
setEnv env = do
    ioBinDb <- liftIO $ eagerlyOnce $ initBinDb env.binDbFilepath
    mbBinDb <- liftIO ioBinDb
    pure $ env { binDb = mbBinDb }  

-- | Initialize a BinDb Trie from a csv data file
--  NOTE: To be used with MonadReader and IORef
initBinDb :: MonadIO m => FilePath -> m (Maybe BinDb)
initBinDb csvFile = do
    csvData <- liftIO $ BL.readFile csvFile
    case CSV.decodeByName csvData of
       Left _err     -> pure Nothing   --- Trie.empty
       Right (_, v) -> do
          r <- liftIO $ newIORef $ Trie.fromList $ V.toList $ V.foldMap toCardMetaTpl v
          pure $ Just r

searchBinDb :: (MonadIO m, MonadReader Env m) => ByteString -> m (Maybe CardMeta)
searchBinDb src = do
    mbBinDbRef <- asks binDb
    case mbBinDbRef of
      Just ioTrie -> do
        trie <- liftIO $ readIORef ioTrie
        pure $ Trie.lookup src trie
      Nothing -> pure Nothing

--- Private API ---

toCardMetaTpl :: BinData -> Vector (ByteString, CardMeta)
toCardMetaTpl cardData = go
  where
    toCardMeta = MkCardMeta cardData.scheme cardData.cardType Nothing Nothing Nothing
    toBs i = Ch.pack $ show i
    go :: Vector (ByteString, CardMeta)
    go = case cardData.iinRangeEnd of
           Nothing -> V.singleton (toBs cardData.iin, toCardMeta)
           Just iinEnd -> V.fromList [ (toBs newIIN, toCardMeta) | newIIN <- [cardData.iin..iinEnd] ]

fromCardTypeField :: ByteString -> CardType
fromCardTypeField = \case
  "credit" -> Credit
  _        -> Debit

fromCardSchemaField :: ByteString -> CardSchema
fromCardSchemaField = \case
    "amex"        -> AmEx
    "diners"      -> DinersInt
    "visa"        -> Visa
    "mastercard"  -> MasterCard
    "discover"    -> Discover
    "unionpay"    -> ChinaUnionPaid
    _             -> Bankcard
