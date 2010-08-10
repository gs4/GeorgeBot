import Network
import System.IO
import System.Environment (getArgs, getProgName)
import Text.Printf
import Control.Monad
import System.Exit


server = "irc.freenode.org"
port = 6667

nick = "strudeler"
realName = "Haskell B. Curry, Speaker for the Parliament"

main = do
  args <- getArgs
  pname <- getProgName
  when (length args /= 1) (putStrLn ("Usage: " ++ pname ++ " <channel>") >> exitWith (ExitFailure (0-1)))
  let chan = args !! 0
  runProg chan

runProg chan' = do
  let chan = if chan' !! 0 == '#' then chan' else '#':chan'
  putStrLn $ "Connecting to channel " ++ chan ++ " on server " ++ server ++ ":" ++ show port
  h <- irc_connect server port
  irc_channel_join h chan listener


irc_connect :: String -> Int -> IO Handle
irc_connect server port = do
  h <- connectTo server ((PortNumber . fromIntegral) port)
  hSetBuffering h NoBuffering
  echo_write h "NICK" nick
  echo_write h "USER" (nick ++ " 0 * :" ++ realName)
  return h
  
irc_channel_join :: Handle -> String -> (Handle -> IO ()) -> IO ()
irc_channel_join handle chan listener_f = do
  echo_write handle "JOIN" chan
  listener_f handle

irc_write :: Bool -> Handle -> String -> String -> IO ()
irc_write echo handle s t = do
  hPrintf handle "%s %s\r\n" s t
  printf       "> %s %s\n"   s t

echo_write = irc_write True

listener :: Handle -> IO ()
listener h = forever $ do
               s <- hGetLine h
               putStrLn s
    where
      forever a = do a; forever a
