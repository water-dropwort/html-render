module Main where

import           Control.Monad              (mapM, unless)
import           Data.Bifunctor
import qualified Data.ByteString            as S
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Either
import           Network.Socket
import           Network.Socket.ByteString  (recv, sendAll)
import           System.Environment         (getArgs)
import           System.IO
import           Text.Parsec.Char
import           Text.Parsec.Prim
import           Text.Parsec.String
import           Text.Parsec.Token
import           Tokenizer
import           TreeConstructor
import           UrlParser

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  -- 引数からリクエスト用のURLデータ生成。成功したら次の処理へ。
  case parseUrl $ head' args of
    Left  msg -> putStrLn msg
    Right url -> do
      -- ソケット生成
      sock <- createSocket url
      -- HTTP リクエスト 送信
      sendAll sock $ C.pack $ buildRequest url
      -- レスポンス受け取り
      res <- recrusiveRecv sock
      close' sock
      -- レスポンスを解析し、それを画面に表示する。
      either putStrLn (mapM_ putStrLn) $ parseResponse $ C.unpack res

head' :: [String] -> Maybe String
head' []    = Nothing
head' (s:_) = Just s

buildRequest :: ParsedUrl -> String
buildRequest url = reqLine ++ headers ++ connection
  where
    reqLine = "GET " ++ path url ++ " HTTP/1.1\r\n"
    headers = "Host: " ++ host url ++ "\r\n"
    connection = "Connection: close" ++ "\r\n\r\n"

createSocket :: ParsedUrl -> IO Socket
createSocket url = do
  let hints = defaultHints { addrSocketType = Stream, addrFamily     = AF_INET }
  addr <- head <$> getAddrInfo (Just hints) (Just $ host url) (Just $ scheme url)
  sock <- socket (addrFamily addr) (addrSocketType addr) defaultProtocol
  connect sock $ addrAddress addr
  return sock

-- 再帰的にソケットからRecvを取得する
recrusiveRecv :: Socket -> IO C.ByteString
recrusiveRecv sock = exec sock $ C.pack ""
  where
    exec :: Socket -> C.ByteString -> IO C.ByteString
    exec s front = do
      res <- recv s 2048
      if C.null res then
        return front
      else
        exec s $ C.concat [front, res]

parseResponse :: String -> Either String [String]
parseResponse s = bimap show (toString 0 . construct) (parse parser "" s)
  where
    -- 最初の < まで消費する。その後、残りに対し tokonize
    parser = skipMany (noneOf "<") >> tokenizer
