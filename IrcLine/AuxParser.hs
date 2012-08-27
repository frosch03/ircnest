module IrcLine.AuxParser
where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char (letter)


commaSep p = p `sepBy` (char ',')
spaceSep p = p `sepBy` (char ' ')

pLetter :: Parser Char
pLetter  = letter

pNumber :: Parser Char
pNumber  = digit

pSpecial :: Parser Char
pSpecial = oneOf "-_[]\\`^{}"

pChstring :: Parser [Char]
pChstring = many1 $ noneOf " \a\0\r\n,"

pNowhite :: Parser [Char]
pNowhite  = many1 $ noneOf " \0\r\n"

pLine :: Parser [Char]
pLine = many1 $ noneOf "\n"


