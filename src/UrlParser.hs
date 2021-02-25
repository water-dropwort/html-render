module UrlParser (
  ParsedUrl,
  parseUrl,
  scheme,
  path,
  host,
  port
  ) where

import           Data.Bifunctor
import           Data.Either
import           Data.Maybe
import           Text.Parsec.Char
import           Text.Parsec.Prim
import           Text.Parsec.String
import           Text.Parsec.Token

-- URLをパースした結果データ
data ParsedUrl = ParsedUrl
                 { scheme :: String
                 , host   :: String
                 , port   :: String
                 , path   :: String
                 } deriving (Show)

parseUrl :: Maybe String -> Either String ParsedUrl
parseUrl (Just url) = first show $ parse urlParser "" url
parseUrl Nothing    = Right defaultParsedUrl


defaultParsedUrl =  ParsedUrl { scheme = "http"
                              , host   = "127.0.0.1:8888"
                              , port   = "8888"
                              , path   = "/index.html"}

urlParser :: Parser ParsedUrl
urlParser =
  do string "http://"
     host <- many $ noneOf "/"
     let port = drop 1 $ dropWhile (/= ':') host
     path <- many $ noneOf "\n"
     return $ ParsedUrl { scheme = "http"
                        , host   = host
                        , port   = port
                        , path   = if path == "" then "/" else path}
