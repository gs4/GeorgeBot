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
                 _port :: PortNumber,
                 _server :: String,
                 _chan :: String,
                 _nick :: String,
                 _uname :: String
                 } deriving (Show)

type BotData = State (Map String String)
type BotConfig = Reader Bot
type BotReaderT = ReaderT Bot IO
type BotAwesome a = StateT (BotData a) BotReaderT

getSocket :: BotConfig Handle
getSocket = _socket `fmap` ask

getPort :: BotConfig PortNumber
getPort = _port `fmap` ask

getServer :: BotConfig String
getServer = _server `fmap` ask

getChan :: BotConfig String
getChan = _chan `fmap` ask

getNick :: BotConfig String
getNick = _nick `fmap` ask

getUname :: BotConfig String
getUname = _uname `fmap` ask

runBot :: BotAwesome () () -> Bot -> IO ()
runBot codes defaultBot = runReaderT (fst `fmap` (runStateT codes emptyBot)) defaultBot

emptyBot :: BotData ()
emptyBot =  put empty

main :: IO ()
main = do
  args <- getArgs
  pname <- getProgName
  when (length args /= 1) (putStrLn ("Usage: " ++ pname ++ " <channel>") >> exitWith (ExitFailure (0-1)))
  let chan = args !! 0
  runProg chan "irc.factset.com" (fromIntegral 6667) "george" "gsommers"

runProg :: String -> String -> PortNumber -> String -> String -> IO ()
runProg chan' server port nick uname = do
  let chan = if chan' !! 0 == '#' then chan' else '#':chan'
  putStrLn $ "Connecting to channel " ++ chan ++ " on server " ++ server ++ ":" ++ show port
  h <- irc_connect server port nick uname
  return ()
  runBot (irc_channel_join chan listener) (Bot h port server chan nick uname)   

irc_connect :: String -> PortNumber -> String -> String -> IO Handle
irc_connect server port nick uname = do
  h <- connectTo server port
  hSetBuffering h NoBuffering
  echo_write h "NICK" nick
  echo_write h "USER" (nick ++ " 0 * :" ++ uname)
  return h
  
irc_channel_join :: String -> (Handle -> IO ()) -> BotAwesome () ()
irc_channel_join chan listener_f = do
  sock <- lift getSocket
--  put conf { _chan = chan }
  echo_write sock "JOIN" chan
  listener_f

irc_write :: Bool -> Handle -> String -> String -> IO ()
irc_write echo handle s t = do
  hPrintf handle "%s %s\r\n" s t
  printf       "> %s %s\n"   s t

echo_write = irc_write True

listener :: Handle -> BotAwesome () ()
listener h = forever $ do
               line <- hGetLine h
               let s = init line
               (handler s) h
               putStrLn s
    where
      forever a = do a; forever a

privmsg :: Handle -> String -> String -> BotAwesome () ()
privmsg h chan s = lift $ lift $ echo_write h "PRIVMSG" (chan ++ " :" ++ s)

handler :: String -> (Handle -> BotAwesome () ())
handler input | "PING :" `isPrefixOf` input = \h -> echo_write h "PONG"  (':' : drop 6 input)
              | otherwise = eval input
                                  
eval :: String -> (Handle -> BotAwesome () ())
eval "!quit" = \h -> echo_write h "QUIT" ":Exiting"
eval ('!':'e':'c':'h':'o':rest) = \h -> privmsg h "chan" rest
eval _ = \h -> return ()


