module Tokenizer (
  tokenizer,
  Token,
  TokenType(..),
  t_type,
  tag_name,
  self_closing,
  char_data)where

import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Prim
import           Text.Parsec.String
import           Text.Parsec.Token

data TokenType = DOCTYPE | START_TAG | END_TAG | CHAR | EOF
               deriving Show

data Token = Token { t_type       :: TokenType
                   , tag_name     :: String
                   , self_closing :: Bool
                   , char_data    :: String
                   } deriving Show

tokenizer :: Parser [Token]
tokenizer = tokenize []

tokenize :: [Token] -> Parser [Token]
tokenize ts =
  try (skipEolAndWS >> eof >> return ts)
  <|> (tokenize' >>= (\t -> tokenize (ts ++ [t])))
  where
    tokenize' = try docType <|> try tag <|> chardata

docType :: Parser Token
docType = do
  skipEolAndWS
  string "<!DOCTYPE "
  skipMany $ noneOf ">"
  char '>'
  return $ Token DOCTYPE "" True ""

tag :: Parser Token
tag = do
  skipEolAndWS
  char '<'
  tokenType <- (char '/' >> return END_TAG) <|> return START_TAG
  name <- many $ noneOf " />\n\r"
  t <- isSelfClose
  return $ Token tokenType name t ""
  where
    isSelfClose = do
      many $ noneOf "/>"
      try (string "/>" >> return True)
        <|> try (string ">" >> return False)
        <|> (anyChar >> isSelfClose)

chardata :: Parser Token
chardata = Token CHAR "" False <$> (skipEolAndWS >> many (noneOf "\n\r<"))

skipEolAndWS :: Parser ()
skipEolAndWS = skipMany $ oneOf "\r\n "
