module Drone 
    ( TnC, Telemetry, Command, Killswitch
    , hatchDrone
    )
where

import Data.List
import Text.Printf

import Network

import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar

import System.IO
import System.Exit

import Prelude hiding (catch)

--Intern 
import Message
import IRC
import Cond

server = "jomach-ich.de"
port   = 6667
chan   = "#test"
nick   = "testProg"

toMsg  = msg nick (Left chan)


type Telemetry  = MVar Message
type Command    = MVar Message
type TnC        = (Telemetry, Command)
type Killswitch = MVar Bool

main :: IO ()
main 
    = do mvMsg  <- newMVar emptyMsg
         mvBack <- newMVar emptyMsg
         mvQuit <- newMVar False
         forkIO (hatchDrone "test" "#test" (mvMsg, mvBack) mvQuit)
         forkIO (tester mvMsg mvQuit)
         forever mvQuit $ do s <- tryTakeMVar mvBack
                             maybe (return ()) (putStrLn . show) s
    where forever mvQuit a = readMVar mvQuit >>= cond (return ())
                                                      (a >> forever mvQuit a)

tester :: Command -> Killswitch -> IO ()
tester mvMsg  mvQuit
    = forever
    $ do x <- getLine
         if x == "quit" then swapMVar mvQuit True >> return ()
                        else putMVar  mvMsg (toMsg x)
    where forever a  = readMVar mvQuit >>= cond (return ())
                                                (a >> forever a)
                                                


hatchDrone :: Nick -> Channel -> TnC -> Killswitch -> IO ()
hatchDrone nick chan (mvMsg, mvBack) mvQuit
    = bracket (ircConnect server port) disconnect runDrone
    where disconnect  = hClose . socket
          runDrone st = runReaderT drone st
          drone       = do ircRegisterNick nick
                           ircJoinChannel  chan
                           ircPrivmsg      (toMsg "hello world ...")
                           h <- asks socket
                           ircIO $ forkIO (readDrone mvBack mvQuit h)
                           forever droneLoop
          toMsg       = msg nick (Left chan)
          droneLoop   = do msg <- ircIO $ tryTakeMVar mvMsg
                           maybe (return ()) ircPrivmsg msg
          forever a  = ircIO (readMVar mvQuit) >>= cond (a >> ircPrivmsg (toMsg "by") >> return ())
                                                        (a >> forever a)
                                                          

readDrone :: Telemetry -> Killswitch -> Handle -> IO ()
readDrone mvBack mvQuit h = forever
    $ do s <- init `fmap` (hGetLine h)
         putMVar mvBack $ parsePRIVMSG s
         if ping s then pong s else (return ())
    where forever a  = readMVar mvQuit >>= cond (return ())
                                                (a >> forever a)
          ping x    = "PING :" `isPrefixOf` x
          pong x    = runReaderT (ircWrite "PONG" (':' : drop 6 x)) (Client h)

parsePRIVMSG :: String -> Message
parsePRIVMSG s = Msg nick (Left chan) msg
    where (nick, s')    = allUntil '!' . drop 1 $ s 
          (host, s'')   = allUntil ' ' . drop 1 $ s'
          (_   , s''')  = allUntil ' ' . drop 1 $ s''
          (chan, s'''') = allUntil ' ' . drop 1 $ s'''
          msg           = drop 2 $ s''''
          allUntil c = (\x -> (takeWhile (/= c) x, dropWhile (/= c) x))




post2IRC :: Message -> IO ()
post2IRC _ = undefined

isIntressting :: Message -> Bool
isIntressting _ = undefined

notifyUser :: IO ()
notifyUser = undefined


