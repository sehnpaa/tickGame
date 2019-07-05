{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Source where

import           Control.Lens
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer     ( decimal )

import           Elements
import           Seconds

data Expr a = SyncPaperclipsWithSeconds a (Paperclips a) | AddPaperclips [Seconds a]

data Source a = Source
  { _sourceText :: Text
  , _sourceStatus :: Text
  , _sourceExpr :: Maybe (Expr a) }
makeLenses ''Source

type Parser = Parsec Void Text

parse :: Integral a => Text -> Maybe (Expr a)
parse = parseMaybe parseExpr

parseExpr :: Integral a => Parser (Expr a)
parseExpr = parseAddPaperclips <|> parseSyncPaperclipsWithSeconds

parseSyncPaperclipsWithSeconds :: Integral a => Parser (Expr a)
parseSyncPaperclipsWithSeconds = do
  _ <- string "if "
  n <- decimal
  _ <- char ';'
  return $ SyncPaperclipsWithSeconds n (Paperclips n)

parseAddPaperclips :: Integral a => Parser (Expr a)
parseAddPaperclips = do
  _  <- string "if second in "
  ns <- listOfNumbers
  _  <- char ';'
  return $ AddPaperclips $ fmap Seconds ns

listOfNumbers :: Integral a => Parser [a]
listOfNumbers = do
  _  <- char '['
  ns <- decimal `sepBy` (char ',')
  _  <- char ']'
  return $ fmap fromInteger ns
