module IrcLine.Datatype
    ( IrcLine(..)
    )
where 

import IrcLine.Sender
import IrcLine.Command

data IrcLine
    = IL { sender  :: Sender
         , command :: Command
         }
    deriving (Show)
