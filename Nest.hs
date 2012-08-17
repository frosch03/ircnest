module Nest 
where

import Data.HashMap
import Data.ByteString.Char8
import Data.Digest.SHA256

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)

import System.IO

import Prelude hiding (lookup)

-- Intern
import Message 
import Drone

chan = "#test"

data Drone 
    = Drone { telemetry  :: Telemetry
            , command    :: Command
            , killswitch :: Killswitch
            }

type DroneClutch = Map Nick Drone
data Nest = Nest { drones  :: DroneClutch
                 , channel :: Channel
                 }

type NestM = StateT Nest IO

nestIO :: IO a -> NestM a
nestIO = liftIO

newNest :: Channel -> Nest
newNest = Nest (Data.HashMap.empty)

newDrone :: Nick -> IO (Nick, Drone)
newDrone nick 
    = do mvTel  <- newMVar emptyMsg
         mvCmd  <- newMVar emptyMsg
         mvQuit <- newMVar False
         return (nick, (Drone mvTel mvCmd mvQuit))

addDrone :: (Nick, Drone) -> Nest -> IO Nest
addDrone (nick, drone) nest
    = do forkIO (hatchDrone nick chan (mvCmd, mvTel) mvQuit) 
         return $ nest { drones = insert nick drone clutch }
    where chan   = channel nest
          clutch = drones  nest
          mvCmd  = command    drone
          mvTel  = telemetry  drone
          mvQuit = killswitch drone
    
withDrone :: Nest -> (Nick, Drone) -> IO Nest
withDrone = flip addDrone

droneFromNick :: Nest -> Nick -> Maybe Drone
droneFromNick nest nick
    = lookup nick clutch
    where clutch = drones nest

killDrone :: Nick -> Nest -> IO Nest
killDrone nick nest
    = maybe 
        (liftIO $ return nest') 
        (\d -> kill d >> return nest') $ mbDrone
    where clutch  = drones nest
          mbDrone = nest `droneFromNick` nick
          nest'   = maybe nest (const $ nest { drones = delete nick clutch }) mbDrone
          kill d  = do putMVar (killswitch d) True

