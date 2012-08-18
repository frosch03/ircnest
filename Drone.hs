module Drone 
    ( TnC, Telemetry, Command, Killswitch, MVTele, MVCmd, MVTnC
    , hatchDrone
    , hatchDrone_
    )
where

import Data.List
import Data.Sequence
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
import qualified Prelude as P

--Intern 
import Message
import IRC
import Cond

server = "jomach-ich.de"
port   = 6667
chan   = "#test"
nick   = "testProg"

toMsg  = msg nick (Left chan)


type Telemetry  = (Seq Message)
type Command    = Message
type TnC        = (Telemetry, Command)
type MVTele     = MVar Telemetry
type MVCmd      = MVar Command
type MVTnC      = (MVTele, MVCmd)
type Killswitch = MVar Bool

main :: IO ()
main 
    = do mvCmd  <- newMVar emptyMsg
         mvTel  <- newMVar Data.Sequence.empty
         mvQuit <- newMVar False
         forkIO (hatchDrone "test" "#test" (mvTel, mvCmd) mvQuit)
         forkIO (tester mvCmd mvQuit)
         forever mvQuit $ do s <- tryTakeMVar mvTel 
                             maybe (return ()) 
                                   (\t -> sequence_ $ map putStrLn $ showTelemetry t) 
                                   $ s
    where forever mvQuit a = readMVar mvQuit >>= cond (return ())
                                                      (a >> forever mvQuit a)

showTelemetry :: Telemetry -> [String]
showTelemetry t = showTel_ (viewr t) [] 

showTel_ :: ViewR Message -> [String] -> [String]
showTel_ EmptyR s = s
showTel_ (ts :> t) s = showTel_ (viewr ts) $ (show t):s


tester :: MVCmd -> Killswitch -> IO ()
tester mvCmd  mvQuit
    = forever
    $ do x <- getLine
         if x == "quit" then swapMVar mvQuit True >> return ()
                        else putMVar  mvCmd (toMsg x)
    where forever a  = readMVar mvQuit >>= cond (return ())
                                                (a >> forever a)
                                                


hatchDrone :: Nick -> Channel -> MVTnC -> Killswitch -> IO ()
hatchDrone nick chan (mvTel, mvCmd) mvQuit
    = do client <- (ircConnect server port) 
         runDrone client
         disconnect client
    where disconnect  = hClose . socket
          runDrone st = runReaderT drone st
          drone       = do ircRegisterNick nick
                           ircJoinChannel  chan
                           ircPrivmsg      (toMsg "hello world ...")
                           h   <- asks socket
                           tID <- ircIO $ forkIO (readDrone mvTel  mvQuit h)
                           forever droneLoop
                           ircIO $ killThread tID
          toMsg       = msg nick (Left chan)
          droneLoop   = do msg <- ircIO $ tryTakeMVar mvCmd
                           maybe (return ()) ircPrivmsg msg
          forever a  = ircIO (readMVar mvQuit) >>= cond (do ircPrivmsg (toMsg "by") 
                                                            return ())
                                                        (a >> forever a)
                                                          

readDrone :: MVTele -> Killswitch -> Handle -> IO ()
readDrone mvTel  mvQuit h = forever
    $ do s <- init `fmap` (hGetLine h)
         isEmptyMVar mvTel 
            >>= cond 
                 (putMVar     mvTel (singleton $ parsePRIVMSG s))
                 (modifyMVar_ mvTel (\t -> return ((parsePRIVMSG s) <| t)))
         if ping s then pong s else (return ())
    where forever a  = readMVar mvQuit >>= cond (return ())
                                                (a >> forever a)
          ping x    = "PING :" `isPrefixOf` x
          pong x    = runReaderT (ircWrite "PONG" (':' : P.drop 6 x)) (Client h)

parsePRIVMSG :: String -> Message
parsePRIVMSG s = Msg nick (Left chan) msg
    where (nick, s')    = allUntil '!' . P.drop 1 $ s 
          (host, s'')   = allUntil ' ' . P.drop 1 $ s'
          (_   , s''')  = allUntil ' ' . P.drop 1 $ s''
          (chan, s'''') = allUntil ' ' . P.drop 1 $ s'''
          msg           = P.drop 2 $ s''''
          allUntil c = (\x -> (takeWhile (/= c) x, dropWhile (/= c) x))




-- Dummy-drone for testing another function
-- (this one is supposed to echo everything)
hatchDrone_ :: Nick -> Channel -> MVTnC -> Killswitch -> IO ()
hatchDrone_ nick chan (mvTel, mvCmd) mvQuit
    = bracket (ircConnect server port) disconnect runDrone
    where disconnect  = hClose . socket
          runDrone st = runReaderT drone st
          drone       = do ircRegisterNick nick
                           ircJoinChannel  chan
                           ircPrivmsg      (toMsg "hello world ...")
                           h <- asks socket
                           ircIO $ forkIO (readDrone mvTel  mvQuit h)
                           forever droneLoop
          toMsg       = msg nick (Left chan)
          droneLoop   = do msg <- ircIO $ tryTakeMVar mvCmd
                           maybe (return ()) ircPrivmsg msg
          forever a  = ircIO (readMVar mvQuit) >>= cond (a >> ircPrivmsg (toMsg "by") >> return ())
                                                        (a >> forever a)

readDrone_ :: MVTele -> Killswitch -> Handle -> IO ()
readDrone_ mvTel  mvQuit h = forever
    $ do s <- init `fmap` (hGetLine h)
         runReaderT (return $ ircPrivmsg (parsePRIVMSG s)) (Client h)
         isEmptyMVar mvTel 
            >>= cond 
                 (putMVar     mvTel (singleton $ parsePRIVMSG s))
                 (modifyMVar_ mvTel (\t -> return ((parsePRIVMSG s) <| t)))
         if ping s then pong s else (return ())
    where forever a  = readMVar mvQuit >>= cond (return ())
                                                (a >> forever a)
          ping x    = "PING :" `isPrefixOf` x
          pong x    = runReaderT (ircWrite "PONG" (':' : P.drop 6 x)) (Client h)

