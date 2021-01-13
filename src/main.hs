import           Data.Either
import           Text.Parsec.String
import           Text.Parsec.Prim
import           Text.Parsec.Token
import           Text.Parsec.Char
import           System.Environment (getArgs)
import           System.IO
import           Network.Socket
import           Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import           Control.Monad (unless)

data ParsedUrl = ParsedUrl
                 { scheme :: String
                 , host   :: String
                 , port   :: String
                 , path   :: String
                 } deriving (Show)


main :: IO ()
main = withSocketsDo $ 
  do args <- getArgs
     case (parseUrl $ head args) of
       Left  msg -> print msg
       Right url ->
         do sock <- createSocket url
            let req = buildRequest url
            putStrLn req
            putStrLn "[DEBUG]Send HTTP request"
            sendAll sock $ C.pack req
            --res <- recrusiveRecv sock ""
            --putStrLn res
            putStrLn "[DEBUG]Create Handle"
            --res <- recrusiveRecv' sock
            --C.putStrLn res
            handle <- socketToHandle sock ReadMode
            hSetBuffering handle LineBuffering
            res <- S.hGetContents handle
            C.putStrLn res
            close' sock
  


parseUrl :: String -> Either String ParsedUrl
parseUrl arg 
  | arg == [] = Right defaultParsedUrl
  | otherwise =
    case (parse urlParser "" arg) of
      Left  res -> Left $ show res
      Right url -> Right url
  where
    defaultParsedUrl :: ParsedUrl
    defaultParsedUrl =  ParsedUrl { scheme = "http"
                                  , host   = "127.0.0.1:8888"
                                  , port   = "8888"
                                  , path   = "/index.html"} 

urlParser :: Parser ParsedUrl
urlParser =
  do string "http://"
     host <- many $ noneOf "/"
     let port = drop 1 $ dropWhile (\c -> c /= ':') host
     path <- many $ noneOf "\n"
     return $ ParsedUrl { scheme = "http"
                        , host   = host
                        , port   = port
                        , path   = if path == "" then "/" else path}

buildRequest :: ParsedUrl -> String
buildRequest url = reqLine ++ headers
  where
    reqLine = "GET " ++ (path url) ++ " HTTP/1.1\r\n"
    headers = "Host: " ++ (host url) ++ "\r\n\r\n"

createSocket :: ParsedUrl -> IO Socket
createSocket url =
  do let hints = defaultHints { addrSocketType = Stream
                              , addrFamily     = AF_INET }
     addr <- head <$> getAddrInfo (Just hints) (Just $ host url) (Just $ scheme url)
     sock <- socket (addrFamily addr) (addrSocketType addr) defaultProtocol
     setSocketOption sock KeepAlive 1
     --withFdSocket sock $ setCloseOnExecIfNeeded
     connect sock $ addrAddress addr
     return sock

recrusiveRecv :: Socket -> String -> IO String 
recrusiveRecv sock res =
  do msg <- recv sock 1024
     if (C.null msg) then
       return res
     else
       recrusiveRecv sock (res ++ (C.unpack msg))

recrusiveRecv' :: Socket -> IO S.ByteString
recrusiveRecv' sock =
  do handle <- socketToHandle sock ReadMode
     --hSetBuffering handle LineBuffering
     res <- doRecrusive handle $ C.pack ""
     hClose handle
     return res
  where
    doRecrusive :: Handle -> S.ByteString -> IO S.ByteString
    doRecrusive h bs =
      do res <- S.hGetNonBlocking h 1024
         C.putStrLn res
         if (C.null res) then
           return bs
         else
           doRecrusive h (S.concat [bs , res])

