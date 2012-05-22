{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Lexer.Class(
       MonadLexer,
       MonadKeyword,
       input,
       lookahead,
       startCode,
       setLexerState,
       setStartCode,
       position,
       keyword,
       addKeyword
       ) where

import Data.Pos

class Monad m => MonadLexer m where
  input :: m String
  lookahead :: m Char
  startCode :: m Int
  position :: m Pos
-- Format: setLexerState input lookahead position
  setLexerState :: String -> Char -> Pos -> m ()
  setStartCode :: Int -> m ()

class Monad m => MonadKeyword t m where
  keyword :: String -> m (Maybe t)
  addKeyword :: String -> t -> m ()