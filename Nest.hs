module Nest 
    ( Nest(..), NestM
    , addDrone
    , killDrone
    , sendByDrone
    , telemetryOfDrone
    , newNest
    , nestIO
    )
where

import Data.Sequence
import Data.Foldable (toList)
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
import Cond

chan = "#test"

data Drone 
    = Drone { telemetry  :: MVTele
            , command    :: MVCmd
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

newDrone :: Nick -> IO (Drone)
newDrone nick 
    = do mvTel  <- newMVar Data.Sequence.empty
         mvCmd  <- newMVar emptyMsg
         mvQuit <- newMVar False
         return (Drone mvTel mvCmd mvQuit)

getClutch :: NestM (DroneClutch)
getClutch = get >>= (\n -> nestIO $ return $ drones n)

getChan :: NestM (Channel)
getChan = get >>= (\n -> nestIO $ return $ channel n)

addDrone :: Nick -> NestM ()
addDrone nick
    = do nest   <- get       
         clutch <- getClutch
         chan   <- getChan
         d      <- nestIO $ newDrone nick
         mvCmd  <- nestIO . return $ command    d
         mvTel  <- nestIO . return $ telemetry  d
         mvQuit <- nestIO . return $ killswitch d

         nestIO $ forkIO (hatchDrone nick chan (mvTel, mvCmd) mvQuit) 
         put $ nest { drones = insert nick d clutch }

    

getDrone :: Nick -> NestM (Maybe Drone)
getDrone nick
    = do nest   <- get
         clutch <- getClutch
         nestIO $ return $ lookup nick clutch

killDrone :: Nick -> NestM ()
killDrone nick
    = do nest    <- get 
         clutch  <- getClutch 
         mbDrone <- getDrone nick
         nest'   <- nestIO $ return $ maybe nest (const $ nest { drones = delete nick clutch }) mbDrone 
         put nest'
         nestIO $ maybe 
           (liftIO $ return ()) 
           (\d -> kill d >> return ()) $ mbDrone
    where kill d  = swapMVar (killswitch d) True

sendByDrone :: Message -> NestM ()
sendByDrone msg
    = do nick    <- nestIO . return $ msgSender msg
         mbDrone <- getDrone nick
         nestIO $ maybe
            (return ())
            (\d -> putMVar (command d) msg) $ mbDrone

telemetryOfDrone :: Nick -> NestM ([Message])
telemetryOfDrone n
    = getDrone n >>= maybe 
            (return ([]))
            (\d -> do mvTel <- return $ telemetry d
                      nestIO $ isEmptyMVar mvTel >>= cond
                            (return ([]))
                            (takeMVar mvTel >>= (\x -> return $ Data.Foldable.toList x))
            )
