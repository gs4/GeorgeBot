import Data.List
import Network
import System.IO
import qualified Data.Map as Map
import System.Environment (getArgs, getProgName)
import Text.Printf
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import System.Exit
import Control.Arrow ((&&&))

data Bot = Bot { _socket :: Handle,
                 _port :: Int,
                 _server :: String,
                 _chan :: String,
                 _nick :: String,
                 _uname :: String
                 } 

type BotKey = String
type BotVal = String

type BotData = Map.Map BotKey BotVal
type BotReaderT = ReaderT Bot IO
type BotAwesome = StateT BotData BotReaderT

runBot :: BotAwesome () -> Bot -> IO ()
runBot codes defaultBot = runReaderT (fst `fmap` (runStateT codes emptyBot)) defaultBot

emptyBot :: BotData
emptyBot =  Map.empty

main :: IO ()
main = do
  args <- getArgs
  pname <- getProgName
  when (length args /= 1) (putStrLn ("Usage: " ++ pname ++ " <channel>") >> exitWith (ExitFailure (0-1)))
  let chan = args !! 0
  runProg chan "irc.factset.com" 6667 "george" "gsommers"

runProg :: String -> String -> Int -> String -> String -> IO ()
runProg chan' server port nick uname = do
  let chan = if chan' !! 0 == '#' then chan' else '#':chan'
  putStrLn $ "Connecting to channel " ++ chan ++ " on server " ++ server ++ ":" ++ show port
  h <- ircConnect server chan port nick uname
  return ()
  runBot (listener) (Bot h port server chan nick uname)   

ircConnect :: String -> String -> Int -> String -> String -> IO Handle
ircConnect server chan port nick uname = do
  h <- connectTo server ((PortNumber . fromIntegral) port)
  hSetBuffering h NoBuffering
  echoWrite h "NICK" nick
  echoWrite h "USER" (nick ++ " 0 * :" ++ uname)
  echoWrite h "JOIN" chan
  return h
  
ircWrite :: Bool -> Handle -> String -> String -> IO ()
ircWrite echo handle s t = do
  hPrintf handle "%s %s\r\n" s t
  printf       "> %s %s\n"   s t

echoWrite = ircWrite True

botWrite :: String -> String -> BotAwesome ()
botWrite s t = do
  sock <- _socket `fmap` ask
  liftIO $ echoWrite sock s t
  
botPut :: BotKey -> BotVal -> BotAwesome ()
botPut key val = modify (Map.insert key val)
                    
botGet :: BotKey -> BotAwesome (Maybe BotKey)
botGet key = (Map.lookup key) `fmap` get
                 

listener :: BotAwesome ()
listener = do sock <- _socket `fmap` ask
              forever $ do
                line <- liftIO $ hGetLine sock
                let s = init line
                handler s
                liftIO $ putStrLn s
        where
          forever a = do a; forever a

privMsg :: String -> BotAwesome ()
privMsg s = do 
  chan <- _chan `fmap` ask
  botWrite "PRIVMSG" (chan ++ " :" ++ s)

handler :: String -> BotAwesome ()
handler input | "PING :" `isPrefixOf` input = botWrite "PONG"  (':' : drop 6 input)
              | otherwise = eval input
                                  
eval :: String -> BotAwesome ()
eval "!quit" = botWrite "QUIT" ":Exiting"
eval ('!':'e':'c':'h':'o':rest) = privMsg rest
eval ('!':'p':'u':'t':rest) = do putToMap rest
                                 chan <- _chan `fmap` ask
                                 botWrite chan "saved!"
eval ('!':'g':'e':'t':rest) = do val <- getFromMap rest
                                 let result = case val of
                                       Nothing -> "null"
                                       Just str -> str
                                 chan <- _chan `fmap` ask
                                 botWrite chan "str"

eval _ = return ()

putToMap :: String -> BotAwesome ()
putToMap rest = botPut key val
  where key = takeWhile (/=' ') rest 
        val = dropWhile (/=' ') rest

getFromMap :: String -> BotAwesome (Maybe BotVal)
getFromMap rest = botGet rest
