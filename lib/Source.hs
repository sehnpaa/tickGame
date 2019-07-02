{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Source where

import           Control.Lens
import           Data.Text
import           Data.Void                      ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

import           Elements

data Expr a = SyncPaperclipsWithSeconds a (Paperclips a)

data Source a = Source
  { _sourceText :: Text
  , _sourceStatus :: Text
  , _sourceExpr :: Maybe (Expr a) }
makeLenses ''Source

type Parser = Parsec Void Text

ff :: Integral a => Text -> Maybe (Expr a)
ff = parseMaybe parseSyncPaperclipsWithSeconds

parseSyncPaperclipsWithSeconds :: Integral a => Parser (Expr a)
parseSyncPaperclipsWithSeconds = do
  _ <- string "if "
  n <- decimal
  _ <- char ';'
  return $ SyncPaperclipsWithSeconds n (Paperclips n)
