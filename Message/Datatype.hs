module Message.Datatype 
    ( Nick
    , Channel
    , Message(..)
    )
where


type Nick    = String
type Channel = String

data Message 
    = Msg { msgSender   :: Nick
          , msgReceiver :: Either Channel Nick
          , msgText     :: String
          }
