{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IRCBot (runBot, Bot (Bot), Hook, Message (PrivMsg, ServMsg), BotKey, BotVal, RandList, ircConnect,
               privMsg, botPut, botGet, botRand, addHook, getSocket, getPort,
               getServer, getChan, getNick, getUname) where 

import Data.List
import Network
import System.IO
import qualified Data.Map as Map
import Text.Printf
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Cont
import System.Random

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
type RandList = [Float]
type Hook = Message -> BotAwesome Message

data BotData = BotData { 
  _rands :: RandList,
  _map   :: (Map.Map BotKey BotVal),
  _hooks :: Hook
  }
               
newtype BotAwesome a = BotAwesome { extractBot :: StateT BotData (ReaderT Bot IO) a }
    deriving (Monad, Functor, MonadIO, MonadReader Bot, MonadState BotData)

---------------------

             {-type    nick   name   host   to     msg   -}
data Message = PrivMsg String String String String String
             | ServMsg String 
             deriving Show

---------------------

getNick :: BotAwesome String
getNick = asks _nick
getUname :: BotAwesome String
getUname = asks _uname
getSocket :: BotAwesome Handle
getSocket = asks _socket
getServer :: BotAwesome String
getServer = asks _server
getChan :: BotAwesome String
getChan = asks _chan
getPort :: BotAwesome Int
getPort = asks _port

runBot :: Bot -> Hook -> IO ()
runBot defaultBot hook = do init <- initialBot hook
                            runReaderT (fst `fmap` (runStateT (extractBot listener) init)) defaultBot

initialBot :: Hook -> IO BotData
initialBot hook = do gen <- getStdGen
                     return $ BotData (randoms gen) Map.empty hook

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
  
modifyRands f (BotData x y z) = BotData (f x) y z
modifyMap f (BotData x y z) = BotData x (f y) z
modifyHooks f (BotData x y z) = BotData x y (f z)
  
botPut :: BotKey -> BotVal -> BotAwesome ()
botPut key val = modify $ (modifyMap $ Map.insert key val)
                    
botGet :: BotKey -> BotAwesome (Maybe BotKey)
botGet key = (Map.lookup key) `fmap` (gets _map)
                 
botRand :: BotAwesome Float
botRand = do x <- head `fmap` gets _rands
             modify $ modifyRands $ tail
             return x

listener :: BotAwesome ()
listener = do sock <- asks _socket
              forever $ do
                line <- liftIO $ hGetLine sock
                let s = init line
                messageHandler $ splitInput s
                liftIO $ putStrLn s
        where
          forever a = do a; forever a

privMsg :: String -> BotAwesome ()
privMsg s = do 
  chan <- _chan `fmap` ask
  botWrite "PRIVMSG" (chan ++ " :" ++ s)

messageHandler :: Message -> BotAwesome ()
messageHandler (ServMsg input) | "PING :" `isPrefixOf` input = botWrite "PONG"  (':' : drop 6 input)
messageHandler (ServMsg _    ) = return ()
messageHandler msg = eval msg

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
eval m@(PrivMsg fnick fname fhost fto fmsg)
--  | fmsg == "!quit" = botWrite "QUIT" ":Exiting"
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
  | "!names" `isPrefixOf` fmsg = do botWrite "NAMES" ""
  | "!rand" `isPrefixOf` fmsg = do let n = read $ (head . tail . words) $ fmsg
                                   x <- botRand
                                   privMsg $ "roll: " ++ (show $ truncate $ n * x)
  | otherwise = gets _hooks >>= (\f -> f m) >> return ()
eval x = return ()

putToMap :: String -> BotAwesome ()
putToMap rest = botPut key val
  where key = takeWhile (/=' ') rest 
        val = dropWhile (/=' ') rest

getFromMap :: String -> BotAwesome (Maybe BotVal)
getFromMap rest = botGet rest

addHook :: (Message -> BotAwesome Message) -> BotAwesome ()
addHook hook = do modify $ modifyHooks $ (composeHooks hook)
    where composeHooks h1 h2 x = h1 x >>= h2





 