module IrcLine.Command.Datatype 
    ( Command(..)
    , Receiver(..)
    , Text
    )
where

import IrcLine.Command.ModeString

type Text = String

data Receiver
    = Channel String
    | Nick    String
    deriving (Show) 



data Command
    = PrivMsg 
        { receivers :: [Receiver] 
        , text      :: Text
        }
    | Join
        { channels :: [Receiver]
        , keys     :: [String]
        }
    | Mode
        { modeline :: ModeString
        }
    | Unknown
        { line :: String
        }
    deriving (Show) 
