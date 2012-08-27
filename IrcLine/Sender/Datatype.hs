module IrcLine.Sender.Datatype
    ( Sender(..)
    , Nick, User, Host
    )
where

type Nick = String
type User = String
type Host = String

data Sender 
    = S { nick :: Nick 
        , user :: User
        , host :: Host
        }
    | Server 
        { host :: Host
        }
    deriving (Show)
