{-# LANGUAGE UndecidableInstances, FlexibleInstances,
	     MultiParamTypeClasses #-}
module Control.Monad.Gensym(
       MonadGensym,
       Gensym,
       GensymT,
       symbol,
       unique,
       runGensym,
       runGensymT,
       ) where

import Control.Monad.Context
import Control.Monad.Lexer
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Gensym.Class
import Control.Monad.Symtab.Class
import Data.Word
import Prelude(Int, IO, Monad, Maybe(..), Bool(..), String, (==), (.), ($), (+), (++), show, length)

import qualified Data.Symbol as Symbol
import qualified Data.HashTable as HashTable

newtype GensymT m a =
  GensymT ((StateT Word (ContextT (HashTable.HashTable String Symbol.Symbol) m)) a)
type Gensym a = GensymT IO a

unpackGensymT (GensymT g) = g

initGensym :: MonadIO m => m (HashTable.HashTable String Symbol.Symbol)
initGensym =
    do
      tab <- liftIO (HashTable.new (==) HashTable.hashString)
      return tab

runGensym :: Gensym a -> IO a
runGensym g = runGensymT g

runGensymT :: MonadIO m => GensymT m a -> m a
runGensymT (GensymT g) =
  do
    tab <- initGensym
    (res, _) <- runContextT (runStateT g Symbol.firstIndex) tab
    return res

symbol' :: MonadIO m => String ->
           (StateT Word (ContextT (HashTable.HashTable String Symbol.Symbol) m)) Symbol.Symbol
symbol' name =
  do
    tab <- context
    result <- liftIO (HashTable.lookup tab name)
    case result of
      Just sym -> return sym
      Nothing ->
        do
          id <- get
          sym <- return (Symbol.symbol id name)
          put (id + 1)
          liftIO (HashTable.insert tab name sym)
          return sym

unique' :: MonadIO m => String ->
           (StateT Word (ContextT (HashTable.HashTable String Symbol.Symbol) m)) Symbol.Symbol
unique' name =
  do
    id <- get
    put (id + 1)
    return (Symbol.symbol id name)

instance Monad m => Monad (GensymT m) where
  return = GensymT . return
  (GensymT x) >>= f = GensymT $ x >>= unpackGensymT . f

instance MonadIO m => MonadGensym (GensymT m) where
  symbol = GensymT . symbol'
  unique = GensymT . unique'

instance MonadIO m => MonadIO (GensymT m) where
  liftIO = GensymT . liftIO

instance MonadTrans GensymT where
  lift = GensymT . lift . lift

instance MonadGensym m => MonadGensym (StateT s m) where
  symbol = lift . symbol
  unique = lift . unique

instance MonadState s m => MonadState s (GensymT m) where
  get = lift get
  put = lift . put

instance MonadLexer m => MonadLexer (GensymT m) where
  input = lift input
  lookahead = lift lookahead
  startCode = lift startCode
  position = lift position
  setLexerState input lookahead = lift . setLexerState input lookahead
  setStartCode code = lift (setStartCode code)

instance MonadKeyword t m => MonadKeyword t (GensymT m) where
  keyword = lift . keyword
  addKeyword str = lift . addKeyword str

instance MonadContext c m => MonadContext c (GensymT m) where
  context = lift context

instance MonadSymtab a m => MonadSymtab a (GensymT m) where
  insert s = lift . (insert s)
  lookup = lift . lookup
  list = lift list
