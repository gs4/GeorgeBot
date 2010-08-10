import Network
import System.IO
import Text.Printf
 
server = "irc.freenode.org"
port   = 6667
chan   = "#tutbot-testing"
nick   = "tutbot"
 
main = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    write h "NICK" nick
    write h "USER" (nick++" 0 * :tutorial bot")
    write h "JOIN" chan
    listen h
 
write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t
 
listen :: Handle -> IO ()
listen h = forever $ do
    s <- hGetLine h
    putStrLn s
  where
    forever a = do a; forever a
