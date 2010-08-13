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



----
{-
  use newtypes (or data's) not just type aliases

  lispy: http://book.realworldhaskell.org/read/monad-transformers.html
   -- this should have a section on it.. (hiding our something)

  goal: add to the state a map of Predicate -> (BotAwesome ())
          for the purpose of hooks

  add support for private messages to users
                  emotes
                  recognizing when someones directed something at the bot (george: hi)
-}                       