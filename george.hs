import Data.List
import Network
import System.IO
import qualified Data.Map as Map
import System.Environment (getArgs, getProgName)
import Text.Printf
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Cont
import System.Exit
import Control.Arrow ((&&&))

----------------------

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

---------------------

             {-type    nick   name   host   to     msg   -}
data Message = PrivMsg String String String String String
             | ServMsg String 
             deriving Show

---------------------

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
  runProg chan "irc.freenode.com" 6667 "george-1442" "gs4"

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
                handler $ splitInput s
                liftIO $ putStrLn s
        where
          forever a = do a; forever a

privMsg :: String -> BotAwesome ()
privMsg s = do 
  chan <- _chan `fmap` ask
  botWrite "PRIVMSG" (chan ++ " :" ++ s)

handler :: Message -> BotAwesome ()
handler (ServMsg input) | "PING :" `isPrefixOf` input = botWrite "PONG"  (':' : drop 6 input)
handler (ServMsg _    ) = return ()
handler msg = eval msg

splitInput :: String -> Message
splitInput line = (`runCont` id) $ do
  callCC $ \exit -> do
    let (nick, rest) = span (\c -> c /= '!') (tail line)
    when (null rest) (exit $ ServMsg line)
    let (name', rest') = span (\c -> c /= '@') (tail rest)
    let name = tail name'
    when (null rest') (exit $ ServMsg line)      
    let (host', cmd') = span (\c -> c /= ' ') rest'
    let host = tail host'
    let cmd = tail cmd'
    return $ makeMessage nick name host cmd
      where
        makeMessage :: String -> String -> String -> String -> Message
        makeMessage nick name host cmd =
          let cmdWords = words cmd
          in case (head cmdWords) of
            "PRIVMSG" -> parsePrivMsg nick name host (unwords $ tail cmdWords)
            _ -> ServMsg line
        parsePrivMsg :: String -> String -> String -> String -> Message
        parsePrivMsg nick name host str =
          let (target:msg') = words str
              msg = tail (unwords msg')
          in PrivMsg nick name host target msg

                                  
eval :: Message -> BotAwesome ()
eval (PrivMsg fnick fname fhost fto fmsg)
  | fmsg == "!quit" = botWrite "QUIT" ":Exiting"
  | "!id" `isPrefixOf` fmsg = privMsg $ (unwords . tail. words) fmsg
  | "!put" `isPrefixOf` fmsg = do let rest = (unwords . tail . words) $ fmsg
                                  putToMap rest
                                  chan <- _chan `fmap` ask
                                  privMsg "saved!"
  | "!get" `isPrefixOf` fmsg = do let rest = (unwords . tail . words) $ fmsg
                                  val <- getFromMap rest
                                  let result = case val of
                                        Nothing -> "null"
                                        Just str -> str
                                  chan <- _chan `fmap` ask
                                  privMsg result
eval _ = return ()

putToMap :: String -> BotAwesome ()
putToMap rest = botPut key val
  where key = takeWhile (/=' ') rest 
        val = dropWhile (/=' ') rest

getFromMap :: String -> BotAwesome (Maybe BotVal)
getFromMap rest = botGet rest
