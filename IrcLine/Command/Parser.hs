module IrcLine.Command.Parser
    ( pCommand
    )
where

import Text.ParserCombinators.Parsec

-- Intern
import IrcLine.AuxParser
import IrcLine.Command.Datatype
import IrcLine.Command.ModeString


pReceiverNick 
    = do c  <- pLetter
         cs <- many (pLetter <|> pNumber <|> pSpecial)
         return (Nick (c:cs))

pReceiverChannel
    = do c  <- oneOf "#&"
         cs <- pChstring
         return (Channel (c:cs))


pReceiver 
    = try pReceiverChannel
      <|> pReceiverNick

pReceivers
    = commaSep pReceiver

pCmdIdentifier
    = try ( do cmdstr <- (many1 $ pLetter)
               return cmdstr
          )
      <|> ( do cmdnum <- (count 3 (oneOf "0123456789")) 
               return cmdnum
          )
          

pCommand :: Parser Command
pCommand
    = try ( do cmd  <- pCmdIdentifier
               case cmd of
                   "PRIVMSG" -> pPrivMsg
                   "JOIN"    -> pJoin
                   "MODE"    -> pMode
                   otherwise -> pUnknown
          )
    
pPrivMsg :: Parser Command
pPrivMsg 
    = do skipMany space
         recs <- pReceivers
         skipMany space
         char ':'
         text <- pLine
         return (PrivMsg recs text)


pJoin :: Parser Command
pJoin 
    = do skipMany space
         char ':'
         chns <- (commaSep pReceiverChannel)
         skipMany space
         keys <- (spaceSep pNowhite)
         return (Join chns keys)

pMode :: Parser Command
pMode 
    = do skipMany space
         line <- pLine
         return (Mode ((read line) :: ModeString))

pUnknown :: Parser Command
pUnknown 
    = do l <- pLine
         return (Unknown l)





