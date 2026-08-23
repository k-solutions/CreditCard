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

import           Country                  (Country)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as Ch
import           Data.Char                (isDigit)
import           Data.Csv                 (FromField (..), FromNamedRecord (..))
import qualified Data.Csv                 as CSV
import           Data.Default             (Default (..))
import           Data.IORef               (IORef)
import           Data.Time.Calendar       (MonthOfYear, Year)
import           Data.Trie                (Trie)
import           GHC.Generics             (Generic)
import           Say                      (sayShow)

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
  deriving (Eq, Generic)

instance Show CCV where
  show _ = "***"

instance Read CCV where
  readsPrec p s =
    readParen (p > 10) (\s0 -> do
      ("MkCCV", s1) <- lex s0
      (n, s2) <- readsPrec 11 s1
      pure (MkCCV n, s2)
    ) s
    ++
    [ (MkCCV n, s1) | (n, s1) <- readsPrec p s, n >= 0 ]
    ++
    [ (MkCCV (read str), s1) | (str, s1) <- readsPrec p s, not (null str), all isDigit str ]

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
  deriving (Show, Eq, Generic)

instance Read CardName where
  readsPrec p s =
    readParen (p > 10) (\s0 -> do
      ("MkCardName", s1) <- lex s0
      (str, s2) <- readsPrec 11 s1
      pure (MkCardName (Ch.pack str), s2)
    ) s
    ++
    [ (MkCardName (Ch.pack str), s1) | (str, s1) <- readsPrec p s ]

newtype CardBrand = MkCardBrand ByteString
  deriving (Show, Eq, Generic)

instance Read CardBrand where
  readsPrec p s =
    readParen (p > 10) (\s0 -> do
      ("MkCardBrand", s1) <- lex s0
      (str, s2) <- readsPrec 11 s1
      pure (MkCardBrand (Ch.pack str), s2)
    ) s
    ++
    [ (MkCardBrand (Ch.pack str), s1) | (str, s1) <- readsPrec p s ]

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
                deriving (Eq, Generic)

instance Show CardNumber where
  show cardNmb = show $ cardNmb.bin <> Ch.map (const '*') masked <> notMasked
    where (masked, notMasked) = BS.splitAt (BS.length cardNmb.accountId - 4) cardNmb.accountId

data CNFields = CNFields
  { fBin       :: !(Maybe ByteString)
  , fAccountId :: !(Maybe ByteString)
  }

parseCNFieldsLoop :: CNFields -> String -> [(CardNumber, String)]
parseCNFieldsLoop acc s = do
  (fld, s1) <- lex s
  ("=", s2) <- lex s1
  case fld of
    "bin" -> do
      (str, s3) <- readsPrec 0 s2
      nextField (acc { fBin = Just (Ch.pack str) }) s3
    "accountId" -> do
      (str, s3) <- readsPrec 0 s2
      nextField (acc { fAccountId = Just (Ch.pack str) }) s3
    _ -> []
  where
    nextField acc' str = case lex str of
      [(",", rest)] -> parseCNFieldsLoop acc' rest
      [("}", rest)] -> case (acc'.fBin, acc'.fAccountId) of
        (Just b, Just a) -> [(MkCardNumber b a, rest)]
        _ -> []
      _ -> []

instance Read CardNumber where
  readsPrec p s =
    -- 1. Positional or record constructor: MkCardNumber ...
    readParen (p > 10) (\s0 -> do
      ("MkCardNumber", s1) <- lex s0
      case lex s1 of
        [("{", s2)] -> parseCNFieldsLoop (CNFields Nothing Nothing) s2
        _ -> do
          (b, s2) <- readsPrec 11 s1
          (acc, s3) <- readsPrec 11 s2
          pure (MkCardNumber (Ch.pack b) (Ch.pack acc), s3)
    ) s
    ++
    -- 2. 2-tuple: ("426398", "2640269299")
    [ (MkCardNumber (Ch.pack b) (Ch.pack acc), s1)
    | ((b, acc), s1) <- readsPrec p s
    , not (null b) && not (null acc)
    ]
    ++
    -- 3. Quoted card number string: "4263982640269299"
    [ (MkCardNumber (Ch.pack b) (Ch.pack a), s1)
    | (str, s1) <- readsPrec p s
    , (b, a) <- maybe [] pure (splitNumberDigits (Ch.pack str))
    ]
    ++
    -- 4. Bare digits: 4263982640269299
    [ (MkCardNumber (Ch.pack b) (Ch.pack a), s1)
    | (tok, s1) <- lex s
    , not (null tok)
    , all isDigit tok
    , (b, a) <- maybe [] pure (splitNumberDigits (Ch.pack tok))
    ]

splitNumberDigits :: ByteString -> Maybe (String, String)
splitNumberDigits bs
  | BS.length bs >= 15 && luhnValid bs =
      let (b, a) = BS.splitAt 6 bs
      in if BS.length a > 8 then Just (Ch.unpack b, Ch.unpack a) else Nothing
  | otherwise = Nothing

luhnValid :: ByteString -> Bool
luhnValid bs =
  let ds = [Ch.index bs i | i <- [0 .. BS.length bs - 1], isDigit (Ch.index bs i)]
  in case reverse (fmap (fromIntegral . \c -> fromEnum c - fromEnum '0') ds :: [Int]) of
       (check : rest) ->
         let total = sum $ zipWith (\f d -> f d) (cycle [luhnDouble, id]) rest
             expectedCheck = (10 - (total `mod` 10)) `mod` 10
         in check == expectedCheck
       _ -> False

luhnDouble :: Int -> Int
luhnDouble n
  | d > 9     = d - 9
  | otherwise = d
  where d = n * 2

-- | Raw credit card input data before validation, with optional extra metadata
data RawCreditCard = MkRawCreditCard
                   { number  :: !ByteString
                   , name    :: !ByteString
                   , validTo :: !ByteString
                   , ccv     :: !ByteString
                   , meta    :: ![ByteString]
                   }
                   deriving (Show, Eq, Generic)

data RawFields = RawFields
  { rNumber  :: !(Maybe ByteString)
  , rName    :: !(Maybe ByteString)
  , rValidTo :: !(Maybe ByteString)
  , rCCV     :: !(Maybe ByteString)
  , rMeta    :: !(Maybe [ByteString])
  }

emptyRawFields :: RawFields
emptyRawFields = RawFields Nothing Nothing Nothing Nothing Nothing

parseRawFieldsLoop :: RawFields -> String -> [(RawCreditCard, String)]
parseRawFieldsLoop acc s = do
  (fld, s1) <- lex s
  ("=", s2) <- lex s1
  case fld of
    "number" -> do
      (val, s3) <- readsPrec 0 s2
      nextField (acc { rNumber = Just (Ch.pack val) }) s3
    "name" -> do
      (val, s3) <- readsPrec 0 s2
      nextField (acc { rName = Just (Ch.pack val) }) s3
    "validTo" -> do
      (val, s3) <- readsPrec 0 s2
      nextField (acc { rValidTo = Just (Ch.pack val) }) s3
    "ccv" -> do
      (val, s3) <- readsPrec 0 s2
      nextField (acc { rCCV = Just (Ch.pack val) }) s3
    "meta" -> do
      (valList, s3) <- readsPrec 0 s2
      nextField (acc { rMeta = Just (fmap Ch.pack valList) }) s3
    _ -> []
  where
    nextField acc' str = case lex str of
      [(",", rest)] -> parseRawFieldsLoop acc' rest
      [("}", rest)] -> case (acc'.rNumber, acc'.rName, acc'.rValidTo, acc'.rCCV) of
        (Just num, Just nm, Just vt, Just c) ->
          let extra = case acc'.rMeta of
                Just m  -> m
                Nothing -> []
          in [(MkRawCreditCard num nm vt c extra, rest)]
        _ -> []
      _ -> []

instance Read RawCreditCard where
  readsPrec p s =
    -- 1. [ByteString] list representation: ["4263982640269299", "John Doe", "12/26", "123", ...]
    [ (MkRawCreditCard (Ch.pack num) (Ch.pack nm) (Ch.pack vt) (Ch.pack c) (fmap Ch.pack extra), s1)
    | (num : nm : vt : c : extra, s1) <- readsPrec p s
    ]
    ++
    -- 2. 4-tuple of String: ("4263982640269299", "John Doe", "12/26", "123")
    [ (MkRawCreditCard (Ch.pack num) (Ch.pack nm) (Ch.pack vt) (Ch.pack c) [], s1)
    | ((num, nm, vt, c), s1) <- readsPrec p s
    ]
    ++
    -- 3. Record or positional constructor: MkRawCreditCard ...
    readParen (p > 10) (\s0 -> do
      ("MkRawCreditCard", s1) <- lex s0
      case lex s1 of
        [("{", s2)] -> parseRawFieldsLoop emptyRawFields s2
        _ -> do
          (num, s2) <- readsPrec 11 s1
          (nm,  s3) <- readsPrec 11 s2
          (vt,  s4) <- readsPrec 11 s3
          (c,   s5) <- readsPrec 11 s4
          case readsPrec 11 s5 of
            [(m, s6)] -> pure (MkRawCreditCard (Ch.pack num) (Ch.pack nm) (Ch.pack vt) (Ch.pack c) (fmap Ch.pack m), s6)
            _         -> pure (MkRawCreditCard (Ch.pack num) (Ch.pack nm) (Ch.pack vt) (Ch.pack c) [], s5)
    ) s

-- | Full credit card info type with metadata information
data CreditCard = MkCreditCard
                { number   :: !CardNumber
                , name     :: !CardName
                , validTo  :: !ValidityDate
                , ccv      :: !CCV
                , metaData :: !(Maybe CardMeta)
                }
                deriving (Show, Eq, Generic)

data CCFields = CCFields
  { fNumber   :: !(Maybe CardNumber)
  , fName     :: !(Maybe CardName)
  , fValidTo  :: !(Maybe ValidityDate)
  , fCCV      :: !(Maybe CCV)
  , fMetaData :: !(Maybe (Maybe CardMeta))
  }

emptyCCFields :: CCFields
emptyCCFields = CCFields Nothing Nothing Nothing Nothing Nothing

parseCCFieldsLoop :: CCFields -> String -> [(CardNumber, CardName, ValidityDate, CCV, Maybe CardMeta, String)]
parseCCFieldsLoop acc s = do
  (fld, s1) <- lex s
  ("=", s2) <- lex s1
  case fld of
    "number" -> do
      (val, s3) <- readsPrec 0 s2
      nextField (acc { fNumber = Just val }) s3
    "name" -> do
      (val, s3) <- readsPrec 0 s2
      nextField (acc { fName = Just val }) s3
    "validTo" -> do
      (val, s3) <- readsPrec 0 s2
      nextField (acc { fValidTo = Just val }) s3
    "ccv" -> do
      (val, s3) <- readsPrec 0 s2
      nextField (acc { fCCV = Just val }) s3
    "metaData" -> do
      ("Nothing", s3) <- lex s2
      nextField (acc { fMetaData = Just Nothing }) s3
    _ -> []
  where
    nextField acc' str = case lex str of
      [(",", rest)] -> parseCCFieldsLoop acc' rest
      [("}", rest)] -> case (acc'.fNumber, acc'.fName, acc'.fValidTo, acc'.fCCV) of
        (Just num, Just nm, Just vt, Just c) ->
          let meta = case acc'.fMetaData of
                Just m  -> m
                Nothing -> Nothing
          in [(num, nm, vt, c, meta, rest)]
        _ -> []
      _ -> []

instance Read CreditCard where
  readsPrec p s =
    -- 1. Parse via RawCreditCard (supports list of strings, 4-tuple, constructor, and record)
    [ (MkCreditCard num nm vt c Nothing, s1)
    | (raw :: RawCreditCard, s1) <- readsPrec p s
    , (b, a) <- maybe [] pure (splitNumberDigits raw.number)
    , let num = MkCardNumber (Ch.pack b) (Ch.pack a)
    , Just nm <- [parseName raw.name]
    , Just vt <- [parseDate raw.validTo]
    , Just c <- [parseCCV raw.ccv]
    ]
    ++
    -- 2. Direct constructor: MkCreditCard ...
    readParen (p > 10) (\s0 -> do
      ("MkCreditCard", s1) <- lex s0
      case lex s1 of
        [("{", s2)] -> do
          (num, nm, vt, c, meta, sEnd) <- parseCCFieldsLoop emptyCCFields s2
          pure (MkCreditCard num nm vt c meta, sEnd)
        _ -> do
          (num, s2)  <- readsPrec 11 s1
          (nm,  s3)  <- readsPrec 11 s2
          (vt,  s4)  <- readsPrec 11 s3
          (c,   s5)  <- readsPrec 11 s4
          ("Nothing", s6) <- lex s5
          pure (MkCreditCard num nm vt c Nothing, s6)
    ) s

parseName :: ByteString -> Maybe CardName
parseName nameInp =
  case Ch.split ' ' nameInp of
    [fstName, midName, lstName] | fstName /= "" && lstName /= "" -> Just $ MkCardName $ mconcat [fstName, " ", midName, " ", lstName]
    [fstName, lstName]          | fstName /= "" && lstName /= "" -> Just $ MkCardName $ mconcat [fstName, " ", lstName]
    _                                                            -> Nothing

parseCCV :: ByteString -> Maybe CCV
parseCCV inp =
  case Ch.readInt inp of
    Just (n, "") | n >= 0 && BS.length inp `elem` [3, 4] -> Just (MkCCV n)
    _ -> Nothing

parseDate :: ByteString -> Maybe ValidityDate
parseDate dateInp =
  case Ch.split '/' dateInp of
    [m, y] -> case (Ch.readInteger m, Ch.readInteger y) of
      (Just (mon, ""), Just (year, ""))
        | mon >= 1 && mon <= 12 && validYear year -> Just (fromIntegral mon, year)
      _ -> Nothing
    _ -> Nothing
  where
    validYear year = any ($ year) [\y -> y >= 1000 && y >= 2020, \y -> y >= 20 && y <= 99]
