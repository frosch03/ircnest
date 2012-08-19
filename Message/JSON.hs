module Message.JSON 
where

import Text.JSON
import Database.CouchDB.JSON ( jsonField, jsonObject, jsonString )

-- Intern
import Message.Datatype

instance JSON (Message) where
    showJSON (Msg n (Left c) t)
        = JSObject $ toJSObject [ ("Nick",    JSString $ toJSString $ n)
                                , ("Channel", JSString $ toJSString $ c)
                                , ("Text",    JSString $ toJSString $ t)
                                ]

    showJSON (Msg n (Right rn) t)
        = JSObject $ toJSObject [ ("Nick",    JSString $ toJSString $ n)
                                , ("RecNick", JSString $ toJSString $ rn)
                                , ("Text",    JSString $ toJSString $ t)
                                ]
    readJSON x 
        = Ok $ (Msg nick recv text)
        where nick = peel x "Nick"
              recv = maybe (Right $ peel x "RecNick") (Left) $ (maybePeel x "Channel")
              text = peel x "Text"



maybePeel js key = (ok2maybe.(jsonField key)) $ fromOK.jsonObject $ js
    where fromOK (Ok x)    = x 
          fromOK (Error x) = error x
          ok2maybe (Ok x)    = Just x
          ok2maybe (Error x) = Nothing


peel js key = (fromOK.(jsonField key)) $ fromOK.jsonObject $ js
    where fromOK (Ok x)    = x 
          fromOK (Error x) = error x

