module Message.Parser
    ( pNick
    , pReceiver
    , pText
    , pMessage
    )
where

import Message.Datatype
import Text.ParserCombinators.Parsec


pLetter  = letter
pNumber  = digit
pSpecial = oneOf "-[]\\`^{}"

pChstring = many1 $ noneOf " \a\0\r\n,"
--pNowhite  = many1 $ noneOf " \0\r\n"

pLine = many1 $ noneOf "\n"



pNick 
    = do c  <- pLetter
         cs <- many (pLetter <|> pNumber <|> pSpecial)
         return (c:cs)


pReceiver 
    = try ( do c  <- oneOf "#&"
               cs <- pChstring
               return (Left (c:cs))
          ) 
      <|> ( do nick <- pNick
               return (Right nick)
          )
      

pText = pLine


pMessage =
    do oneOf ":"
       n <- pNick
       char '!'
       manyTill anyChar (try (string "PRIVMSG "))
       r <- pReceiver
       string " :"
       t <- pText
       return (Msg n r t)
