{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Data.CreditCard.Internal
  ( module Data.CreditCard.Internal.Types
  , module Data.CreditCard.Internal.Errors
  , module Data.CreditCard.Internal.Helpers
  , module Data.CreditCard.Internal.Parser
  , setEnv
  , initBinDb
  , searchBinDb
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as Ch
import qualified Data.ByteString.Lazy             as BL
import qualified Data.Csv                         as CSV
import           Data.CreditCard.Internal.Errors
import           Data.CreditCard.Internal.Helpers
import           Data.CreditCard.Internal.Parser
import           Data.CreditCard.Internal.Types
import           Data.IORef
import qualified Data.Trie                        as Trie
import           Data.Vector                      (Vector)
import qualified Data.Vector                      as V
import           System.IO.Memoize                (eagerlyOnce)

-- | A default environment 
setEnv :: MonadIO m => Env -> m Env
setEnv env = do
  ioBinDb <- liftIO $ eagerlyOnce $ initBinDb env.binDbFilepath
  mbBinDb <- liftIO ioBinDb
  pure $ env { binDb = mbBinDb }  

-- | Initialize a BinDb Trie from a csv data file
-- NOTE: To be used with MonadReader and IORef
initBinDb :: MonadIO m => FilePath -> m (Maybe BinDb)
initBinDb csvFile = do
  csvData <- liftIO $ BL.readFile csvFile
  case CSV.decodeByName csvData of
    Left _err    -> pure Nothing
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
      Nothing     -> V.singleton (toBs cardData.iin, toCardMeta)
      Just iinEnd -> V.fromList [ (toBs newIIN, toCardMeta) | newIIN <- [cardData.iin..iinEnd] ]
