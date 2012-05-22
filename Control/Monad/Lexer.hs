{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances,
             MultiParamTypeClasses #-}
module Control.Monad.Lexer(
       MonadLexer,
       MonadKeyword,
       Lexer,
       LexerT,
       runLexerT,
       runLexer,
       input,
       lookahead,
       startCode,
       setLexerState,
       setStartCode,
       position,
       keyword,
       addKeyword
       ) where

import Control.Monad.Lexer.Class
import Control.Monad.State(StateT, runStateT, get, put)
import Control.Monad.Trans
import Data.Pos
import Data.Word

import Data.HashTable(HashTable)
import qualified Data.HashTable as HashTable

-- (input, lookahead, state, commentDepth, filename)
data LexerState t =
  LexerState { psInput :: String, psStartCode :: !Int,
               psKeywordTab :: HashTable String t,
               psPos :: !Pos }

newtype LexerT t m a = LexerT ((StateT (LexerState t) m) a)
type Lexer t a = LexerT t IO a

unpackLexerT (LexerT pm) = pm

-- Format: runLexerT filename input m
runLexerT :: MonadIO m =>
             [(String, t)] -> String -> String -> LexerT t m a -> m a
runLexerT keywords filename input (LexerT pm) =
  do
    tab <- liftIO (HashTable.fromList HashTable.hashString keywords)
    (res, _) <- runStateT pm (LexerState { psInput = '\n' : input,
                                           psStartCode = 0, psKeywordTab = tab,
                                           psPos = point filename 1 1 0 })
    return res

runLexer :: [(String, t)] -> String -> String -> Lexer t a -> IO a
runLexer keywords filename input m = runLexerT keywords filename input m

input' :: Monad m => (StateT (LexerState t) m) String
input' =
  do
    st <- get
    return (tail (psInput st))

lookahead' :: Monad m => (StateT (LexerState t) m) Char
lookahead' =
  do
    st <- get
    return (head (psInput st))

startCode' :: Monad m => (StateT (LexerState t) m) Int
startCode' =
  do
    st <- get
    return (psStartCode st)

position' :: Monad m => (StateT (LexerState t) m) Pos
position' =
  do
    st <- get
    return (psPos st)

setLexerState' :: Monad m =>
                  String -> Char -> Pos -> (StateT (LexerState t) m) ()
setLexerState' input lookahead pos =
  do
    st <- get
    put (st { psPos = pos, psInput = (lookahead :input) })

setStartCode' :: Monad m => Int -> (StateT (LexerState t) m) ()
setStartCode' startCode =
  do
    st <- get
    put (st { psStartCode = startCode })

keyword' :: MonadIO m => String -> (StateT (LexerState t) m) (Maybe t)
keyword' str =
  do
    st <- get
    liftIO (HashTable.lookup (psKeywordTab st) str)

addKeyword' :: MonadIO m => String -> t -> (StateT (LexerState t) m) ()
addKeyword' str tok =
  do
    st <- get
    liftIO (HashTable.insert (psKeywordTab st) str tok)

instance Monad m => Monad (LexerT t m) where
  return = LexerT . return
  (LexerT x) >>= f =
    LexerT $ x >>= unpackLexerT . f

instance MonadIO m => MonadLexer (LexerT t m) where
  input = LexerT input'
  lookahead = LexerT lookahead'
  startCode = LexerT startCode'
  position = LexerT position'
  setLexerState input lookahead = LexerT . setLexerState' input lookahead
  setStartCode = LexerT . setStartCode'

instance MonadIO m => MonadKeyword t (LexerT t m) where
  keyword = LexerT . keyword'
  addKeyword str = LexerT . addKeyword' str

instance MonadIO m => MonadIO (LexerT t m) where
  liftIO = LexerT . liftIO

instance MonadTrans (LexerT t) where
  lift = LexerT . lift