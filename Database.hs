module Database 
    ( newDroneDB
    , deleteDroneDB
    , postIntoDroneDB
    )
where

import Text.JSON

import Database.CouchDB
import Database.CouchDB.JSON

import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Char (toLower)

import Control.Monad.IO.Class (liftIO)


-- Intern
import Config

import Message

type PublishMessage = CouchMonad (Either String Rev)
type FetchCouch a = CouchMonad [(Doc, a)] 
type DeleteCouch  = CouchMonad Bool
type KeyCouch     = CouchMonad [String]
type CouchQuery   = (FetchCouch Message, ([Message] -> [String]))


runCouch = runCouchDBURI


newDroneDB :: Nick -> IO ()
newDroneDB n = runCouch dBase $ createDB (map toLower n)

deleteDroneDB :: Nick -> IO ()
deleteDroneDB n = runCouch dBase $ dropDB (map toLower n) >> return ()

postIntoDroneDB :: Message -> IO ()
postIntoDroneDB msg = runCouch dBase $ publishPosting msg >> return ()


publishPosting :: Message -> PublishMessage
publishPosting msg@(Msg nick _ _) =
    do now     <- liftIO getPOSIXTime
       docName <- return $ doc.show $ now 
       newNamedDoc (db (map toLower nick)) docName msg
