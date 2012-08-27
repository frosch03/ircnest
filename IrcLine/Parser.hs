module IrcLine.Parser
where

import Text.ParserCombinators.Parsec

-- Intern
import IrcLine.AuxParser
import IrcLine.Datatype
import IrcLine.Sender
import IrcLine.Sender.Parser (pSender)
import IrcLine.Command
import IrcLine.Command.Parser (pCommand)


pIrcLine :: Parser IrcLine
pIrcLine
    = do char ':'
         s <- pSender
         (try (char ' '))
         c <- pCommand
         return (IL s c)
