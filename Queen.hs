module Queen
where

import Data.List
import Data.Sequence hiding (drop)

import Control.Monad.State

-- Intern 
import Message
import Nest
import Cond



main :: IO ()
main 
    = do testNest <- return $ newNest "#test"
         evalStateT (nestPart) testNest
    where nestPart :: NestM ()
          nestPart 
            = loopWhile eval
            where loopWhile a 
                    = do line <- nestIO $ getLine 
                         nestIO (return $ "quit" `isPrefixOf` line) >>= cond
                                    (return ())
                                    (a line >> loopWhile a)


eval :: String -> NestM ()
eval s
    | "new drone" `isPrefixOf` s
    = addDrone $ drop 10 $ s

    | "kill drone" `isPrefixOf` s
    = killDrone $ drop 11 $ s

    | "read drone" `isPrefixOf` s
    = telemetryOfDrone (drop 11 $ s) >>= (\m -> nestIO $ sequence_ $ map (putStrLn . show) m)

    | "send via" `isPrefixOf` s 
    = get >>= (\c -> sendByDrone (msg (channel c)))
    where msg  c = Msg nick (Left c) txt
          nick   = takeWhile ((/=) ' ') . drop 9 $ s
          txt    = drop 1 . dropWhile ((/=) ' ') . drop 9 $ s
