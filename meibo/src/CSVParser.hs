module CSVParser (parseCSV, Entry, CSV) where

import Control.Applicative ((<*),(*>))
import Text.Parsec
import Text.Parsec.String (Parser)

type Entry = [String]
type CSV = [Entry]

parseCSV :: String -> Either String CSV
parseCSV xs = case parse csv "parseCSV" xs of
    Left _  -> Left "error"
    Right r -> Right r

{-
RFC 4180

csv = 1*(record CRLF)
record = field *(COMMA field)
field = (escaped / non-escaped)
escaped = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE
non-escaped = *TEXTDATA
TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E
COMMA = %x2C
CRLF = CR LF
CR = %x0D
LF = %x0A
DQUOTE = %x22
-}

csv :: Parser [[String]]
csv = sepBy1 record crlf

record :: Parser [String]
record = sepBy1 field comma

field :: Parser String
field = escaped <|> nonEscaped

escaped :: Parser String
escaped = dquote *>
          many (textdata <|> comma <|> cr <|> lf
            <|> try (dquote *> dquote))
          <* dquote

nonEscaped :: Parser String
nonEscaped = many textdata

textdata :: Parser Char
textdata = oneOf (" !" ++ ['#'..'+'] ++ ['-'..'~'])

comma :: Parser Char
comma = char ','

crlf :: Parser Char
crlf = cr *> lf

lf :: Parser Char
lf = char '\x0a'

cr :: Parser Char
cr = char '\x0d'

dquote :: Parser Char
dquote = char '"'
