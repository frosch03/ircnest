module Message 
    ( Message(..)
    , Nick
    , Channel
    , emptyMsg
    , msg
    ) 
where 

import Message.Datatype
import Message.Instances
import Message.JSON

emptyMsg :: Message
emptyMsg = Msg "" (Left "") ""

msg :: Nick -> Either Channel Nick -> String -> Message
msg = Msg
