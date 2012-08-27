module IrcLine.Sender.Parser
where

import Text.ParserCombinators.Parsec

-- Intern
import IrcLine.AuxParser
import IrcLine.Sender.Datatype


pNick :: Parser Nick
pNick 
    = do c  <- pLetter
         cs <- many (pLetter <|> pNumber <|> pSpecial)
         return (c:cs)

pUser :: Parser User 
pUser = many1 $ noneOf "@"

pHost :: Parser Host
pHost = many1 $ noneOf " "


pPrefix
    = try ( do n <- pNick
               char '!'
               u <- pUser
               char '@'
               h <- pHost
               return (S n u h)
          )
      <|> ( do s <- pHost
               return (Server s)
          )

pSender :: Parser Sender
pSender = pPrefix
