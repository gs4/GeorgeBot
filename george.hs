import Data.List
import Network
import System.IO
import Data.Map
import System.Environment (getArgs, getProgName)
import Text.Printf
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import System.Exit

data Bot = Bot { _socket :: Handle,
                 _port :: Int,
                 _server :: String,
                 _chan :: String,
                 _nick :: String,
                 _uname :: String
                 } 

type BotData = State (Map String String)
type BotConfig = Reader Bot
type BotReaderT = ReaderT Bot IO
type BotAwesome = StateT (BotData ()) BotReaderT

getSocket :: BotConfig Handle
getSocket = _socket `fmap` ask

getPort :: BotConfig Int
getPort = _port `fmap` ask

getServer :: BotConfig String
getServer = _server `fmap` ask

getChan :: BotConfig String
getChan = _chan `fmap` ask

getNick :: BotConfig String
getNick = _nick `fmap` ask

getUname :: BotConfig String
getUname = _uname `fmap` ask

runBot :: BotAwesome () -> Bot -> IO ()
runBot codes defaultBot = runReaderT (fst `fmap` (runStateT codes emptyBot)) defaultBot

emptyBot :: BotData ()
emptyBot =  put empty

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
  h <- irc_connect server chan port nick uname
  return ()
  runBot (listener) (Bot h port server chan nick uname)   

irc_connect :: String -> String -> Int -> String -> String -> IO Handle
irc_connect server chan port nick uname = do
  h <- connectTo server ((PortNumber . fromIntegral) port)
  hSetBuffering h NoBuffering
  echo_write h "NICK" nick
  echo_write h "USER" (nick ++ " 0 * :" ++ uname)
  echo_write h "JOIN" chan
  return h
  
irc_write :: Bool -> Handle -> String -> String -> IO ()
irc_write echo handle s t = do
  hPrintf handle "%s %s\r\n" s t
  printf       "> %s %s\n"   s t

echo_write = irc_write True

bot_write :: String -> String -> BotAwesome ()
bot_write s t = do
  sock <- _socket `fmap` ask
  lift $ lift $ echo_write sock s t

listener :: BotAwesome ()
listener = do sock <- _socket `fmap` ask
              forever $ do
                line <- lift $ lift $ hGetLine sock
                let s = init line
                (handler s) sock
                lift $ lift $ putStrLn s
        where
          forever a = do a; forever a

privmsg :: String -> BotAwesome ()
privmsg s = do 
  chan <- _chan `fmap` ask
  bot_write "PRIVMSG" (chan ++ " :" ++ s)

handler :: String -> (Handle -> BotAwesome ())
handler input | "PING :" `isPrefixOf` input = \h -> (bot_write "PONG"  (':' : drop 6 input))
              | otherwise = eval input
                                  
eval :: String -> (Handle -> BotAwesome ())
eval "!quit" = \h -> bot_write "QUIT" ":Exiting"
eval ('!':'e':'c':'h':'o':rest) = \h -> privmsg rest
eval _ = \h -> return ()


