module Sound.Tidal.Trigger.Parse (parseInput) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lex.Integral as LI
import qualified Data.ByteString.Lex.Fractional as LF
import Data.Attoparsec.ByteString.Char8 (Parser, space, inClass, isDigit, isSpace, sepBy, parseOnly, takeWhile1, takeTill, try, endOfInput, string, char, takeByteString, isAlpha_ascii)
import Data.Maybe
import Data.Word
import Control.Applicative
import Data.String

import Sound.Tidal.Trigger.Types


input :: Parser [Input]
input = do
  vals <- inputValues
  takeByteString
  return vals

--endOfLine :: Parser

inputValues :: Parser [Input]
inputValues = inputValue `sepBy` space

inputValue :: Parser Input
inputValue = ivdouble <|> ivint <|> ivstring <|> fail "unparseable"

ivdouble :: Parser Input
ivdouble = IVDouble <$> fst.fromJust <$> LF.readSigned LF.readDecimal <$> double

double :: Parser B.ByteString
double = do
  s <- try (string $ fromString "-")
  a <- takeWhile1 isDigit
  b <- string $ fromString "."
  c <- takeWhile1 isDigit
  return $ B.concat [s,a,b,c]

signedint :: Parser B.ByteString
signedint = do
  s <- try (string $ fromString "-")
  a <- int
  return $ B.concat [s, a]

ivint :: Parser Input
ivint = IVInt <$> fst.fromJust <$> LI.readSigned LI.readDecimal <$> (signedint <|> int)

int :: Parser B.ByteString
int = do
  a <- takeWhile1 isDigit
  return a

ivstring :: Parser Input
ivstring = do
  d <- takeWhile1 (inClass "a-zA-Z0-9")
  return $ IVString $ B.unpack d

parseInput str = either (const []) (id) $ parseOnly input $ fromString str
