{- Copyright 2008 Uwe Hollerbach <uh@alumni.caltech.edu>
Portions of this were derived from Jonathan Tang's haskell
tutorial "Write yourself a scheme in 48 hours" and are thus
Copyright Jonathan Tang (but there isn't much of his stuff left).

This file is part of haskeem.
haskeem is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

haskeem is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with haskeem; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

$Id: parser.hs,v 1.17 2010-01-05 05:23:34 uwe Exp $ -}

module FrameworkHs.SExpReader.Parser (readExpr, readExprList, readNumber, specialSymbolChars, lexFile) where
import Prelude
import Data.Char
import Data.Ratio
import Text.ParserCombinators.Parsec as TPCP hiding (spaces)
import Control.Monad.Error as CME
import qualified Data.IntMap as DIM
import Control.Monad (mapM_)
import System.Environment

import FrameworkHs.SExpReader.LispData

lexFile :: FilePath -> IO (ThrowsError [LispVal])
lexFile f = do
  c <- readFile f
  return $ readExprList c

-- Parsers for the various kinds of LispVal

-- "#!/some/path/to/executable" at the top of the file, to enable
-- scheme "shell" scripts: for the rest of the program, it's a comment

hashbang :: Parser Char
hashbang = char '#' >> char '!' >> many (noneOf "\r\n") >> return ' '

-- semicolon to end-of-line, the oldest style of lisp comment

comment :: Parser Char
comment = 
  do char ';'
     many (noneOf "\r\n")
     return ' '

spaces :: Parser ()
spaces = skipMany1 (comment <|> space)

-- This is not quite R6RS-compliant: R6RS allows '.'

specialSymbolChars :: String
specialSymbolChars = "!$%&*+-/:<=>?@^_~."

symbol :: Parser Char
symbol = oneOf specialSymbolChars

-- This is a small extension to R6RS

controlChar :: Parser Char
controlChar =
  do char '^'
     c <- oneOf (['A' .. 'Z'] ++ "[\\]^_")
     return (chr (ord c + 1 - ord 'A'))

namedChar :: Parser Char
namedChar =
  do name <- string "alarm"
          <|> string "backspace"
          <|> string "delete"
          <|> string "esc"
          <|> string "linefeed"
          <|> TPCP.try (string "newline")
          <|> string "nul"
          <|> string "page"
          <|> string "return"
          <|> string "space"
          <|> string "tab"
          <|> string "vtab"
     case name of
          "nul"       -> return (chr 0)
          "alarm"     -> return (chr 7)
          "backspace" -> return (chr 8)
          "tab"       -> return '\t'
          "linefeed"  -> return '\n'
          "newline"   -> return '\n'
          "vtab"      -> return (chr 11)
          "page"      -> return (chr 12)
          "return"    -> return '\r'
          "esc"       -> return (chr 27)
          "space"     -> return ' '
          "delete"    -> return (chr 127)
          _ -> progError "namedChar"

parseChar :: Parser LispVal
parseChar =
  do char '#'
     char '\\'
     c <- TPCP.try controlChar <|> TPCP.try namedChar <|> anyChar
     return (Char c)

-- This is not quite R6RS-compliant: R6RS requires a hex escape spec,
-- and it forbids the "otherwise" clause below. oh well... later maybe

escChar :: Parser Char
escChar =
  do char '\\'
     c <- anyChar
     return (case c of
             'a' -> chr 7
             'b' -> chr 8
             't' -> '\t'
             'n' -> '\n'
             'v' -> chr 11
             'f' -> chr 12
             'r' -> '\r'
             _ -> c)

parseBool :: Parser LispVal
parseBool =
  do char '#'
     v <- oneOf "tTfF"
     return (case v of
            't' -> lispTrue
            'T' -> lispTrue
            'f' -> lispFalse
            'F' -> lispFalse
            _ -> progError "parseBool")

parseString :: Parser LispVal
parseString =
  do char '"'
     x <- many (escChar <|> noneOf "\"")
     char '"'
     return (String x)

parseSymbol :: Parser LispVal
parseSymbol =
  do first <- letter <|> symbol
     rest <- many (letter <|> digit <|> symbol)
     return (Symbol (first:rest))

readBaseInt :: Integer -> String -> Integer
readBaseInt b s = foldl ma 0 s
                  where ma v1 v2 = b*v1 + toInteger (digitToInt v2)

readBaseFrac :: Integer -> String -> Double
readBaseFrac _ [] = 0.0
readBaseFrac b s = r * foldr ma 0 s where
                   r = 1.0 / fromInteger b
                   ma v1 v2 = fromIntegral (digitToInt v1) + r*v2

parseHdr :: Parser (Char, Integer)
parseHdr =
  do b <- option 'd' (char '#' >> oneOf "bodxBODX")
     s <- option '+' (oneOf "+-")
     let base = case b of
                 'b' -> 2
                 'B' -> 2
                 'o' -> 8
                 'O' -> 8
                 'd' -> 10
                 'D' -> 10
                 'x' -> 16
                 'X' -> 16
                 _ -> progError "parseHdr"
     return (s, base)

baseDigits :: Integer -> String
baseDigits 2  = "01"
baseDigits 8  = "01234567"
baseDigits 10 = "0123456789"
baseDigits 16 = "0123456789abcdefABCDEF"
baseDigits _  = progError "baseDigits"

int :: String
int = "int"

-- The fact that this parser can deal with floating-point numbers
-- in bases 2, 8, and 16 as well as 10 is an extension of R6RS.

-- Parse first alternative for floating-point numbers: \d+(\.\d*)?

parseF1 :: Integer -> Parser (String,String)
parseF1 b =
  do ip <- many1 (oneOf (baseDigits b))
     fp <- option int (char '.' >> many (oneOf (baseDigits b)))
     return (ip,fp)

-- Parse second alternative for floating-point numbers: \.\d+

parseF2 :: Integer -> Parser (String,String)
parseF2 b =
  do char '.'
     fp <- many1 (oneOf (baseDigits b))
     return ("0",fp)

-- Parse the exponent

parseExp :: Integer -> Parser Integer
parseExp b =
  do oneOf (if b == 16 then "xX" else "eExX")
     s <- option '+' (oneOf "+-")
     num <- many1 (oneOf (baseDigits b))
     let e = readBaseInt b num
     return (if s == '-' then (-e) else e)

powi :: Integer -> Integer -> Integer
powi b e | e == 0    = 1
         | e < 0     = error "negative exponent in powi"
         | even e    = powi (b*b) (e `quot` 2)
         | otherwise = b * powi b (e - 1)

pow :: Integer -> Integer -> Double
pow b e =
  if e >= 0 then fromInteger (powi b e) else recip (fromInteger (powi b (-e)))

-- Parse an integer or a floating-point number. This parser will return
-- numbers written as aaaEbb (with no decimal point) as integers, if the
-- exponent bb is non-negative.

parseIntOrFlt :: Parser LispVal
parseIntOrFlt =
  do (s, b) <- parseHdr
     (ip, fp) <- parseF1 b <|> parseF2 b
     e <- option 0 (parseExp b)
     let fpi = if fp == int then "0" else fp
         vf = pow b e * (fromInteger (readBaseInt b ip) + readBaseFrac b fpi)
         vi = powi b e * readBaseInt b ip
     if fp == int && e >= 0
        then return (IntNumber (if s == '-' then (-vi) else vi))
        else return (FltNumber (if s == '-' then (-vf) else vf))

-- Parse a rational number written as numerator/denominator. This parser
-- accepts and understands rational infinity, both positive and negative,
-- and rational not-a-number: +infinity is written as 1/0, -infinity as
-- -1/0, and not-a-number as 0/0. That's an incompatible extension of R6RS.

parseRat :: Parser LispVal
parseRat =
  do (s, b) <- parseHdr
     nstr <- many1 (oneOf (baseDigits b))
     char '/'
     dstr <- many1 (oneOf (baseDigits b))
     let num = readBaseInt b nstr
         den = readBaseInt b dstr
         ns = if s == '-' then (-num) else num
         val = if den /= 0
                  then ns % den
                  else if ns > 0
                       then myRatPInf
                       else if ns < 0
                            then myRatNInf
                            else myRatNaN
     if denominator val == 1
        then return (IntNumber (numerator val))
        else return (RatNumber val)

-- Parse a couple of special floating-point numbers mandated by R6RS

parseNaNInf :: Parser LispVal
parseNaNInf =
  do val <- TPCP.try (string "+nan.0")
        <|> TPCP.try (string "-nan.0")
        <|> TPCP.try (string "+inf.0")
        <|> TPCP.try (string "-inf.0")
     case val of
          "+nan.0" -> return (FltNumber myFltNaN)
          "-nan.0" -> return (FltNumber myFltNaN)
          "+inf.0" -> return (FltNumber myFltPInf)
          "-inf.0" -> return (FltNumber myFltNInf)
          _ -> progError "parseNaNInf"

parseNumber :: Parser LispVal
parseNumber = TPCP.try parseNaNInf <|> TPCP.try parseRat <|> parseIntOrFlt

-- Parsers for the abbreviations for the various kinds of quoting entities:
--	'<datum>   =>  (quote <datum>)
--	`<datum>   =>  (quasiquote <datum>)
--	,<datum>   =>  (unquote <datum>)
--	,@<datum>  =>  (unquote-splicing <datum>)

parseQQ :: Parser LispVal
parseQQ =
  do char '`'
     x <- parseExpr
     return (List [Symbol "quasiquote", x])

parseQ :: Parser LispVal
parseQ =
  do char '\''
     x <- parseExpr
     return (List [Symbol "quote", x])

parseUQ :: Parser LispVal
parseUQ =
  do char ','
     x <- parseExpr
     return (List [Symbol "unquote", x])

parseUQS :: Parser LispVal
parseUQS =
  do char ','
     char '@'
     x <- parseExpr
     return (List [Symbol "unquote-splicing", x])

parseQuoted :: Parser LispVal
parseQuoted = TPCP.try parseUQS
          <|> TPCP.try parseUQ
          <|> TPCP.try parseQQ
          <|> parseQ

-- Parser for a dotted-list or a regular list. Due to the representation of
-- scheme lists as haskell lists rather than as dotted-pairs, it's slightly
-- tricky to get the case of (a . (b . (c . ()))) and similar forms to come
-- out right; however, that is explicitly described as exactly identical to
-- the list (a b c) according to the RnRS standard, so it has to be treated
-- correctly.

parseList :: String -> Parser LispVal
parseList [o,c] =
  do char o
     skipMany space
     hd <- sepEndBy parseExpr spaces
     tl <- option (List []) (TPCP.try (char '.' >> spaces >> parseExpr))
     skipMany space
     char c
     if isl tl
        then return (List (hd ++ unpl tl))
        else if isdl tl
                then return (DottedList (hd ++ unpdlh tl) (unpdlt tl))
                else return (DottedList hd tl)
  where isl (List ((Symbol sym):_)) =
          not (sym == "unquote" || sym == "unquote-splicing")
        isl (List _) = True
        isl _ = False
        unpl (List l) = l
        unpl _ = progError "parseDottedList/unpl"
        isdl (DottedList _ _) = True
        isdl _ = False
        unpdlh (DottedList h _) = h
        unpdlh _ = progError "parseDottedList/unpdlh"
        unpdlt (DottedList _ t) = t
        unpdlt _ = progError "parseDottedList/unpdlt"

parseParList :: Parser LispVal
parseParList = parseList "()"

parseBraList :: Parser LispVal
parseBraList = parseList "[]"

-- Parser for a vector: this is similar to a list (but not a dotted-list),
-- except that R6RS says access times are generally faster than for lists.
-- It would seem that haskell Arrays would be the natural way to go, but
-- the documentation for those is... well, crappy. Data.IntMap is much
-- better documented, and pretty close to what we want. Access times aren't
-- O(1), but they are O(min(n,W)), where n is the size of the vector and
-- W is the size in bits of a machine word: either 32 or 64 usually. This
-- is due to the implementation of Data.IntMap: internally, it's a PATRICIA
-- tree. That should be fast enough for the moment; if it becomes an issue,
-- I can always change later. Data.IntMaps are extensible, so I could in
-- principle have extensible vectors, which would mean I'd not need to store
-- the length, but bounds-checked arrays seem like a nice feature to have;
-- I can add an explicit grow-vector routine, which as a result of the
-- extensibility of Data.IntMaps will be very easy to write.

parseVector :: Parser LispVal
parseVector =
  do char '#'
     char '('
     skipMany space
     vals <- sepBy parseExpr spaces
     skipMany space
     char ')'
     return (Vector (toInteger (length vals))
                    (DIM.fromAscList (addkey 0 vals)))
  where addkey _ [] = []
        addkey n (v:vs) = (n, v) : addkey (n+1) vs

parseExpr :: Parser LispVal
parseExpr = parseString
        <|> TPCP.try parseBool
        <|> TPCP.try parseChar
        <|> TPCP.try parseNumber
        <|> TPCP.try parseVector
        <|> TPCP.try parseSymbol
        <|> parseQuoted
        <|> parseParList
        <|> parseBraList

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input =
    case parse parser "lisp" input of
         Left err -> throwError (Parser err)
         Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList =
  readOrThrow (optional hashbang >> skipMany spaces >>
               endBy parseExpr (spaces <|> eof))

-- Parser for just numbers, for internally converting strings to numbers;
-- it's just a little more lenient than only and exactly a number: allow
-- whitespace on either side, that doesn't harm anything and seems polite

parseJustNumber :: Parser LispVal
parseJustNumber =
  do skipMany space
     num <- parseNumber
     skipMany space
     eof
     return num

readNumber :: String -> ThrowsError LispVal
readNumber input =
    case parse parseJustNumber "number" input of
         Left _ -> return lispFalse
         Right val -> return val
