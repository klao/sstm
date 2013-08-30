{-# LANGUAGE DeriveDataTypeable #-}

module Control.Concurrent.SSTM (
  SSTM
  , sTest   -- Mainly for testing
  , toSignalled
  , readTChanSig
  , readTMVarSig
  , waitBlockedSig

  , BlockVar
  , newBlockVarIO
  , signalledOnBlockVar
  , waitBlocked

  , BlockRegistry
  , newBlockRegistryIO
  , registerBlockVar
  , Signalled
  , runSignalled
  , runSignalledUnsafe
  , signalled
  , waitAllBlocked
  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Typeable

--------------------------------------------------------------------------------

-- Read it as "Signalling STM".
--
-- sTest should be a non-blocking no-sideeffects transaction that
-- returns True iff sOp would block.
data SSTM a = SSTM { sTest :: STM Bool, _sOp :: STM a }

instance Functor SSTM where
  {-# INLINE fmap #-}
  fmap f (SSTM t op) = SSTM t (fmap f op)

instance Applicative SSTM where
  {-# INLINE pure #-}
  pure = SSTM (return False) . pure
  {-# INLINE (<*>) #-}
  (SSTM tf f) <*> (SSTM ta a) = SSTM ((||) <$> tf <*> ta) (f <*> a)

instance Alternative SSTM where
  {-# INLINE empty #-}
  empty = SSTM (return True) empty
  {-# INLINE (<|>) #-}
  (SSTM ta a) <|> (SSTM tb b) = SSTM ((&&) <$> ta <*> tb) (a <|> b)


-- General translation of an STM transaction into an SSTM. More
-- efficient implementations can be written for specific cases.

data SSTMTranslation = SSTMTranslation  deriving (Show, Typeable)
instance Exception SSTMTranslation

toSignalled :: STM a -> SSTM a
{-# INLINABLE toSignalled #-}
toSignalled op = SSTM test op
  where
    -- This would be the naive implementation, but the problem is that
    -- it has side-effects if op has side effects and doesn't block.
    -- test = ((op >> return False) <|> return True)
    --
    -- This implementation is equivalent, but currently GHC has a bug
    -- that prohibits us using this one: http://ghc.haskell.org/trac/ghc/ticket/8035
    --
    -- test = ((op >> throwSTM SSTMTranslation) <|> return True)
    --        `catchSTM` (\SSTMTranslation -> return False)

    test = ((op >> throwSTM SSTMTranslation)
            `catchSTM` (\SSTMTranslation -> return False)) <|> return True

-- Useful specific SSTMs:

readTChanSig :: TChan a -> SSTM a
{-# INLINE readTChanSig #-}
readTChanSig ch = SSTM (isEmptyTChan ch) (readTChan ch)

readTMVarSig :: TMVar a -> SSTM a
{-# INLINE readTMVarSig #-}
readTMVarSig var = SSTM (isEmptyTMVar var) (readTMVar var)

waitBlockedSig ::  BlockVar -> SSTM ()
{-# INLINE waitBlockedSig #-}
waitBlockedSig bvar = SSTM (not <$> isBlocked bvar) (waitBlocked bvar)


--------------------------------------------------------------------------------

newtype BlockVar = BV (TVar (STM Bool))

newBlockVarIO :: IO BlockVar
{-# INLINE newBlockVarIO #-}
newBlockVarIO = BV <$> newTVarIO (return False)

signalledOnBlockVar :: BlockVar -> SSTM a -> IO a
{-# INLINE signalledOnBlockVar #-}
signalledOnBlockVar (BV tvar) (SSTM test op) = do
  atomically $ writeTVar tvar test
  atomically $ op <* writeTVar tvar (return False)


-- The following two functions are equivalent with the following isomorphism:
-- isBlocked v = waitBlocked v >> return True <|> return False
-- waitBlocked v = isBlocked v >>= check

isBlocked :: BlockVar -> STM Bool
{-# INLINE isBlocked #-}
isBlocked (BV tvar) = join $ readTVar tvar

waitBlocked :: BlockVar -> STM ()
{-# INLINE waitBlocked #-}
waitBlocked bvar = isBlocked bvar >>= check

--------------------------------------------------------------------------------

newtype BlockRegistry = BR (TVar [BlockVar])

newBlockRegistryIO :: IO BlockRegistry
newBlockRegistryIO = BR <$> newTVarIO []

registerBlockVar :: BlockRegistry -> BlockVar -> IO ()
{-# INLINE registerBlockVar #-}
registerBlockVar (BR registry) bvar = atomically $ modifyTVar' registry (bvar:)


-- TODO(klao): We shouldn't fix the underlying monad, we just need it
-- to be monadIO.
type Signalled a = ReaderT BlockVar IO a

runSignalled :: BlockRegistry -> Signalled a -> IO a
runSignalled registry prog = do
  bvar <- newBlockVarIO
  registerBlockVar registry bvar
  runReaderT prog bvar

runSignalledUnsafe :: Signalled a -> IO a
runSignalledUnsafe prog = do
  bvar <- newBlockVarIO
  runReaderT prog bvar

-- This is the equivalent of 'atomically' in our Signalled STM world.
signalled :: SSTM a -> Signalled a
{-# INLINE signalled #-}
signalled op = do
  bvar <- ask
  lift $ signalledOnBlockVar bvar op

waitAllBlocked :: BlockRegistry -> STM ()
waitAllBlocked (BR registry) =
  mapM_ waitBlocked =<< readTVar registry
