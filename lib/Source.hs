{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Source where

import           Control.Lens
import           Control.Arrow                  ( left )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Void                      ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer     ( decimal )

import           Elements
import           Seconds

data Expr a = SyncPaperclipsWithSeconds a (Paperclips a) | AddPaperclips [Seconds a]

data CustomParseError = NothingToParse | CPE String

newtype SourceText = SourceText { unSourceText :: Text }

instance Show SourceText where
  show (SourceText a) = unpack a

newtype SourceStatus = SourceStatus Text

instance Show SourceStatus where
  show (SourceStatus a) = unpack a

data Source a = Source
  { _sourceText :: SourceText
  , _sourceStatus :: SourceStatus }
makeLenses ''Source

type Parser = Parsec Void Text

parse :: Integral a => Text -> Either CustomParseError (Expr a)
parse t =
  left (\x -> if t == "" then NothingToParse else CPE $ errorBundlePretty x)
    $ Text.Megaparsec.parse parseExpr "" t

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
