import IRCBot

import System.Exit
import System.Environment
import Data.List
import Control.Monad

main :: IO ()
main = do
  args <- getArgs
  pname <- getProgName
  when (length args /= 1) (putStrLn ("Usage: " ++ pname ++ " <channel>") >> exitWith (ExitFailure (0-1)))
  let chan = args !! 0
  runProg chan "irc.freenode.com" 6667 "karavaev" "gs4"

runProg :: String -> String -> Int -> String -> String -> IO ()
runProg chan' server port nick uname = do
  let chan = if chan' !! 0 == '#' then chan' else '#':chan'
  putStrLn $ "Connecting to channel " ++ chan ++ " on server " ++ server ++ ":" ++ show port
  h <- ircConnect server chan port nick uname
  runBot (Bot h port server chan nick uname) georgeHooks

georgeHooks :: Hook
georgeHooks msg = return msg >>= v2w >>= randomQuestions >>= doEmotes >>= askWhy
                  >>= doEmotes >>= disagree >>= disagree2 >>= disagree3
                    
v2w m@(PrivMsg nick name host to msg) = 
  do if "vodka" `elem` (words . stripPunctuation) msg
       then privMsg "Wodka?"
       else return ()
     return m
     
randomQuestions m@(PrivMsg nick name host to msg) =
  do x <- botRand
     if x <= 0.05
       then do let ws = (words . stripPunctuation) msg
               y <- botRand
               let n = truncate $ fromIntegral (length ws) * y
               let w = ws !! n
               privMsg $ w ++ "?"
       else return ()
     return m
     
     
doEmotes m@(PrivMsg nick name host to msg) = 
  do x <- botRand
     if x <= 0.01
       then privMsg "guys..."
       else return ()
     return m
     
askWhy m@(PrivMsg nick name host to msg) =     
  do myName <- asks _nick
     if (myName `isInfixOf` msg)
       then privMsg "what?"
       else return ()
     return m

disagree m@(PrivMsg nick name host to msg) = 
  do x <- botRand
     if x <= 0.01
       then privMsg "that's not right"
       else return ()
     return m

disagree2 m@(PrivMsg nick name host to msg) = 
  do x <- botRand
     if x <= 0.01
       then privMsg "no?"
       else return ()
     return m

disagree3 m@(PrivMsg nick name host to msg) = 
  do x <- botRand
     if x <= 0.01
       then privMsg "incorrect"
       else return ()
     return m

     
stripPunctuation [] = []
stripPunctuation (a:str) = if a `elem` "!.,:;?-()-_=+~'\"[]{}\\|<>/" 
                            then stripPunctuation str 
                            else a:(stripPunctuation str)
                                 