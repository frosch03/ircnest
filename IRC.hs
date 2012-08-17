module IRC
    ( Client(..)
    , Net

    , ircRegisterNick
    , ircJoinChannel
    , ircConnect
    , ircPrivmsg
    , ircWrite
    , ircIO
    )
where


import Text.Printf

import Network

import Control.Monad.Reader
import Control.Exception (bracket_)

import System.IO

--
-- Intern
import Message


data Client = Client { socket :: Handle }
type Net = ReaderT Client IO




ircRegisterNick :: Nick -> Net ()
ircRegisterNick nick = 
    do ircWrite "NICK" nick
       ircWrite "USER" (nick ++ " 0 * :irc-drone of (" ++ nick ++ ")")

ircJoinChannel :: Channel -> Net ()
ircJoinChannel = ircWrite "JOIN"


-- Connect to the server and return the initial bot state
ircConnect :: String -> Int -> IO Client
ircConnect server port = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Client h)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a


ircPrivmsg :: Message -> Net () 
ircPrivmsg (Msg _ (Left c) s) = ircWrite "PRIVMSG" (c ++ " :" ++ s)


ircWrite :: String -> String -> Net () 
ircWrite s t = do 
    h <- asks socket
    ircIO $ hPrintf h "%s %s\r\n" s t


ircIO :: IO a -> Net a
ircIO = liftIO
