module Message.Instances
where

import Text.ParserCombinators.Parsec (parse)

import Message.Datatype
import Message.Parser

instance Read (Message) where
    readsPrec p s 
        = case parse (pMessage) "" s of
            Left  _ -> error $ "error while parsing Message"
            Right x -> [(x, "")]

privmsg = "PRIVMSG "
seperat = " :"

instance Show (Message) where
    show msg =  msgSender msg ++ " "
             ++ privmsg
             ++ receiver msg
             ++ seperat
             ++ msgText msg
        where receiver = either id id . msgReceiver 
