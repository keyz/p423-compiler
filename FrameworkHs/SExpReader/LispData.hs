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

$Id: lispdata.hs,v 1.21 2010-01-05 05:23:34 uwe Exp $ -}

module FrameworkHs.SExpReader.LispData
    (LispVal( Symbol
            , Boolean
            , Char
            --, Delay
            , DottedList
            , IntNumber
            , RatNumber
            , FltNumber
            --, Func
            --, IOPrim
            , List
            --, Port
            --, Prim
            --, Socket
            , String
            , Vector
            ),
     LispError(NumArgs, TypeMismatch, Parser, BadSpecial, NotFunction,
               UnboundVar, DefaultErr, OutOfRange, VectorBounds, UserException,
               DelCont),
     ThrowsError, Env, PureEnv, IOThrowsError, liftThrows,
     myRatPInf, myRatNInf, myRatNaN, myFltPInf, myFltNInf, myFltNaN,
     lispTrue, lispFalse,
     EEnv(EEnv), eeE, eePE, eeDC, eeQL, eeNewE, eeNewPE, eeNewDC,
     eeQLIncr, eeQLDecr, progError) where
import Prelude
import System.IO
import System.IO.Error hiding (try)
import Data.Char
import Data.Ratio()
import Text.ParserCombinators.Parsec as TPCP hiding (spaces)
import Control.Monad.Error as CME
import Data.IORef
import qualified Data.IntMap as DIM
import GHC.Real -- for the :% operator, which does not reduce the fractions
--import Network

-- Define everything we are going to work with, and how to display it

-- rational infinities and NaN: gotta take care with these,
-- they don't always play nice with other numbers

myRatPInf, myRatNInf, myRatNaN :: Rational
myRatPInf = 1:%0
myRatNInf = (-1):%0
myRatNaN = 0:%0

-- floating-point infinities and NaN

myFltPInf, myFltNInf, myFltNaN :: Double
myFltPInf = 1.0e99 ** 1.0e99	-- probably big enough...
myFltNInf = -myFltPInf
myFltNaN = sqrt (-1.0)

-- An environment: a table connecting names of objects and values

type Env = IORef [(String, IORef LispVal)]

-- A pure env: just strings and LispVals

type PureEnv = [(String, LispVal)]

-- A lisp value: these are the stuff which normally gets processed

data LispVal = Symbol String
             | Boolean Bool
             | IntNumber Integer
             | RatNumber Rational
             | FltNumber Double
             | String String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Char Char
             -- | Prim ([LispVal] -> ThrowsError LispVal)
             -- | IOPrim ([LispVal] -> IOThrowsError LispVal)
             -- | Func {params :: [String],
             --        vararg :: (Maybe String),
             --        body :: [LispVal],
             --        closure :: Env,
             --        name :: (Maybe String),
             --        macro :: Bool,
             --        cont :: Bool}
             -- | Delay {obj :: LispVal,
             --          closure :: Env,
             --          tag :: String}
             -- | Port Handle
             | Vector Integer (DIM.IntMap LispVal)

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Symbol atom) = atom
showVal (Boolean True) = "#t"
showVal (Boolean False) = "#f"
showVal (IntNumber num) = show num
showVal (RatNumber num) =
  show (numerator num) ++ "/" ++ show (denominator num)
showVal (FltNumber num) = show num
showVal (String str) = "\"" ++ str ++ "\""
showVal (List lst) = "(" ++ unwords (map showVal lst) ++ ")"
showVal (DottedList lst cab) = "(" ++ unwords (map showVal lst)
                               ++ " . " ++ showVal cab ++ ")"
-- the control-character printing and parsing assumes ASCII
showVal (Char ch) | ch == chr 0   = "#\\nul"
                  | ch == chr 7   = "#\\alarm"
                  | ch == chr 8   = "#\\backspace"
                  | ch == '\t'    = "#\\tab"
                  | ch == '\n'    = "#\\linefeed"
                  | ch == chr 11  = "#\\vtab"
                  | ch == chr 12  = "#\\page"
                  | ch == '\r'    = "#\\return"
                  | ch == chr 27  = "#\\esc"
                  | ch == ' '     = "#\\space"
                  | ch == chr 127 = "#\\delete"
                  | isControl ch  = "#\\^" ++ [chr (ord ch + ord 'A' - 1)]
                  | isPrint ch    = "#\\" ++ [ch]
                  | otherwise     = [ch]
--showVal (Prim _) = "<primitive>"
--showVal (Func {params = args, vararg = varargs, body = _,
--               closure = _, name = nm, macro = ismac, cont = _}) =
--  let lopen = if ismac then "<macro" else "(lambda"
--      lclose = if ismac then ">" else ")"
--      inner = lopen ++ " (" ++ unwords args ++
--              (case varargs of
--                    Nothing -> ""
--                    Just arg -> " . " ++ arg) ++ ") " ++
--              "..." ++ lclose
--  in case nm of
--          Nothing -> inner
--          Just val -> "(" ++ val ++ " . " ++ inner ++ ")"
--showVal (IOPrim _) = "<IO primitive>"
--showVal (Port _) = "<IO port>"
--showVal (Socket _) = "<IO socket>"
--showVal (Delay {obj = o, closure = _, tag = _}) = "<promise>" ++ show o

showVal (Vector _ vals) =
  "#(" ++ unwords (map showVal (DIM.elems vals)) ++ ")"

-- True and False at the scheme level

lispTrue, lispFalse :: LispVal
lispTrue = Boolean True
lispFalse = Boolean False

-- A lisp error: these get processed when an error of some kind occurs;
-- a DelCont isn't an error, but it is an unusual condition...

data LispError = NumArgs String Integer [LispVal]
               | TypeMismatch String String LispVal
               | Parser ParseError
               | BadSpecial String LispVal
               | NotFunction String LispVal
               | UnboundVar String String
               | DefaultErr String
               | OutOfRange String Double
               | VectorBounds Integer LispVal
               | UserException LispVal
               | DelCont LispVal

instance Show LispError where show = showError

instance Error LispError where
  noMsg = DefaultErr "An error has occurred"
  strMsg = DefaultErr

showError :: LispError -> String
showError (NumArgs func expected found) =
  func ++ " expected " ++ show expected ++ " args; got values \"" ++
  unwords (map showVal found) ++ "\""
showError (TypeMismatch func expected found) =
  func ++ " expected " ++ expected ++ " args; got " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecial msg form) = msg ++ ": \"" ++ show form ++ "\""
showError (NotFunction msg func) = msg ++ ": " ++ show func
showError (UnboundVar msg var) = msg ++ ": " ++ var
showError (DefaultErr msg) = msg
showError (OutOfRange func val) = func ++ " arg out of range: " ++ show val
showError (VectorBounds len n) =
  "vector index out of bounds: " ++ show n ++
  " not in [0.." ++ show (len - 1) ++ "]"
showError (UserException val) = "user exception " ++ show val
showError (DelCont val) = "delcont value " ++ show val

type ThrowsError = Either LispError

type IOThrowsError = ErrorT LispError IO

-- convert a ThrowsError foo value into an IOThrowsError foo value

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- An extended environment: environment plus delimited continuations stuff
-- plus quote-level stuff plus whatever else we may need later

data EEnv = EEnv Env PureEnv [[LispVal]] Integer

eeE :: EEnv -> Env
eeE  (EEnv e _ _ _) = e

eePE :: EEnv -> PureEnv
eePE (EEnv _ e _ _) = e

eeDC :: EEnv -> [[LispVal]]
eeDC (EEnv _ _ d _) = d

eeQL :: EEnv -> Integer
eeQL (EEnv _ _ _ q) = q

eeNewE :: EEnv -> Env -> EEnv
eeNewE  (EEnv _ pe d q) e = EEnv e pe d q

eeNewPE :: EEnv -> PureEnv -> EEnv
eeNewPE (EEnv e _ d q) pe = EEnv e pe d q

eeNewDC :: EEnv -> [[LispVal]] -> EEnv
eeNewDC (EEnv e pe _ q) d = EEnv e pe d q

eeQLIncr, eeQLDecr :: EEnv -> EEnv
eeQLIncr (EEnv e pe d q) = EEnv e pe d (q + 1)
eeQLDecr (EEnv e pe d q) = EEnv e pe d (q - 1)

progError :: String -> a
progError = error . ("internal error at " ++)
