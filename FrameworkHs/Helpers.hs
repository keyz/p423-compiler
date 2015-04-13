{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,FlexibleContexts,DeriveDataTypeable #-}
module FrameworkHs.Helpers
  (
    -- * Types for compiler configuration and construction
    P423Config (.. )
  , PassM, getConfig, runPassM, orPassM
  , P423Exception ( AssemblyFailedException
                  , ParseErrorException
                  , ASTParseException
                  , NoValidTestsException
                  , NoInvalidTestsException
                  , PassFailureException
                  , WrapperFailureException
                  )
  , shortExcDescrip
  , passFailure,  passFailureM
--  , parseFailure
  , parseFailureM
  , P423Pass ( P423Pass
             , pass
             , passName
             , wrapperName
             , trace
             )
  , Option (..)
  
  -- * Helpers for representations
  , fixnumBits
  , shiftFixnum
  , maskFixnum
  , maskVector
  , maskPair
  , maskProcedure
  , maskBoolean
  , tagFixnum
  , tagPair
  , tagProcedure
  , tagVector
  , tagBoolean
  , tagNonfixnum
  , repTrue
  , repFalse
  , repNil
  , repVoid
  , dispCar
  , dispCdr
  , dispVectorData
  , dispVectorLength
  , dispProcedureData
  , dispProcedureCode
  , sizePair
  -- * An alternative `Show` class for printing to X86 assembly code:
  , X86Print, format
  , OpCode

  -- * Emitting text to a handle
  , GenM, Gen, gen, genLn, genJustLn
  , hPutGenM, runGenM, showGen
  , emitOp1, emitOp2, emitOp3
  , emitLabelLabel
  , emitLabel
  , emitJumpLabel, emitJump
  , emitEntry, emitExit
               
  -- * Shorthands for common emissions:
  , pushq, popq
  , movq, leaq

  -- * Pretty printing:
  , PP(..), ppSexp, pppSexp

  -- * Parsing
  , parseListWithFinal
  , parseUVar
  , parseFVar
  , parseLabel
  , parseRelop
  , parseBinop
  , parseReg
  , parseInt32
  , parseInt64
  , parseValPrim, parseEffectPrim, parsePredPrim
    
  -- * Misc numeric and string helpers
  , isInt32
  , isInt64
  , isUInt6
  , isFixnum
  , wordShift
  , ash
  , chomp
  ) where

import Prelude hiding (LT, EQ, GT)
import Blaze.ByteString.Builder as BBB
import Blaze.ByteString.Builder.Char8 (fromChar, fromString, fromShow)
import Data.List (intersperse)
import Data.Set (size, fromList)
import Data.Char (isDigit, isSpace, isAlpha)
import Data.Int
import Data.Bits
import Data.ByteString (ByteString, hPut)
-- import Data.ByteString (ByteString, hPut)
import Data.ByteString.Char8 (unpack)
import Control.Monad (unless, mapM_)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error
import qualified Data.Set as S
import Text.Parsec.Error (ParseError)
import qualified Text.PrettyPrint.HughesPJ as P
import Control.Exception
import Data.Typeable
import System.IO

import FrameworkHs.Prims
import FrameworkHs.SExpReader.LispData

data P423Config =
  P423Config
    { framePointerRegister :: Reg
    , allocationPointerRegister :: Reg
    , returnAddressRegister :: Reg
    , returnValueRegister :: Reg
    , parameterRegisters :: [Reg]
    , runWrappers :: Bool  
    }

-- | A monad for implementing passes.  It provides access to the global
-- configuration, and also handles errors.
type PassM = ReaderT P423Config (Either String)

-- | Getting the configuration
getConfig :: (MonadReader P423Config m) => m P423Config
getConfig = ask

-- | A compiler pass with metadata
data P423Pass a b =
  P423Pass
    { pass :: P423Config -> a -> b       -- ^ The implementation of the pass
    , passName :: String                 -- ^ The canonical name of the pass
    , wrapperName :: String              -- ^ The name of the "wrapper" for
                                         --   interpreting the pass's output
    , trace :: Bool                      -- ^ Debug this pass?
    }

-- | This runs a PassM computation with a given configuration.  Any uncaught failures
-- becoming true Haskell errors.
runPassM :: P423Config -> PassM a -> a
runPassM conf m =
  case runReaderT m conf of
    Left str -> error str
    Right x  -> x

-- | Backtracking.  If the first action throws an exception, try the second.
orPassM :: PassM a -> PassM a -> PassM a
orPassM m1 m2 = do
  cfg <- getConfig
  case runReaderT m1 cfg of
    Left _  -> m2
    Right x -> return x
  
-- | Throwing an error inside a compiler pass.
passFailureM :: String -> String -> PassM a
passFailureM who e = lift $ Left (who ++ ": " ++ e)
-- passFailureM = return . Left . PassFailureException ""

-- | Throwing an error, non-monadic version.
passFailure :: String -> String -> a
passFailure who e = throw $ PassFailureException who e


-- | Optional information
data Option a = Default | Option a

split :: Char -> String -> (String,String)
split s [] = ([],[])
split s (c:cs)
  | (c == s)  = ([],cs)
  | otherwise = (c:before,cs')
    where (before,cs') = split s cs

------------------------------------------------------------
-- Exceptions ----------------------------------------------

data P423Exception = AssemblyFailedException String
                   | ASTParseException String
                   | ParseErrorException ParseError
                   | NoValidTestsException
                   | NoInvalidTestsException
                   | PassFailureException String String
                   | WrapperFailureException String String
                   deriving (Typeable)

instance Exception P423Exception
instance Show P423Exception where
  show e@(AssemblyFailedException e')   = shortExcDescrip e ++ ": " ++ show e'
  show e@(ParseErrorException e')       = shortExcDescrip e ++ ": " ++ show e'
  show e@(ASTParseException s)          = shortExcDescrip e ++ ": " ++ s
  show e@(NoValidTestsException)        = shortExcDescrip e
  show e@(NoInvalidTestsException)      = shortExcDescrip e
--  show e@(PassFailureException p e')    = shortExcDescrip e ++ ": " ++ e'
  show e@(PassFailureException p e')    = shortExcDescrip e 
  show e@(WrapperFailureException w e') = shortExcDescrip e ++ ": " ++ e'

shortExcDescrip :: P423Exception -> String
shortExcDescrip e = case e of
  (AssemblyFailedException e)   -> "Assembly failure"
  (ParseErrorException pe)      -> "SExp parse failure in tests"
  (ASTParseException s)         -> "AST parse failure"
  (NoValidTestsException)       -> "Couldn't find valid tests"
  (NoInvalidTestsException)     -> "Couldn't find invalid tests"
  (PassFailureException p e)    -> "Pass failure, " ++ p ++ ": " ++ e
  (WrapperFailureException w e) -> "Wrapper failure (" ++ w ++ ")"


------------------------------------------------------------
-- Emitting ------------------------------------------------

-- | Implementation type for a code generator
type GenM = WriterT Builder PassM

-- | A code generator with only an output and no result
type Gen = GenM ()

-- | Add to the output of a generator
gen :: (X86Print a) => a -> Gen
gen a = tell $ format a

-- | Add a newline along with the given output
genLn :: (X86Print a) => a -> Gen
genLn a = do
  gen a
  genJustLn
  
genJustLn :: Gen
genJustLn = tell $ fromChar '\n'

-- | Put the output of running a generator action to a handle
hPutGenM :: P423Config -> GenM a -> Handle -> IO ()
hPutGenM c g h = case runReaderT (runWriterT g) c of
  Left s -> throwIO (userError $ "Error during code generation: " ++ s)
  Right (_, b) -> hPut h $ BBB.toByteString b

-- | Get the result and output of a generator action
runGenM :: P423Config -> GenM a -> Either String (a, ByteString)
runGenM c g = case runReaderT (runWriterT g) c of
  Left s -> Left s
  Right (x, bu) -> Right (x, BBB.toByteString bu)

-- | Given a P423Config, show the output of a Gen
showGen :: P423Config -> Gen -> String
showGen c g = show $ case runGenM c g of
  Left s -> "Error: " ++ s
  Right ((), b) -> show b

class X86Print a where
  format :: a -> Builder

instance Show Builder where
  show = show . toByteString 

instance X86Print Builder where
  format = id

instance X86Print ByteString where
  format = pp

instance X86Print String where
  format = pp

instance X86Print Integer where
  format i = fromString "$" `mappend` pp i

instance X86Print Reg where
  format r = fromString "%" `mappend` pp r

instance X86Print Label where
  format (L name ind) = mconcat [fromString  "L", pp ind, fromString  "(%rip)"]

instance X86Print Disp where
  format (D reg off) = mconcat [pp off, fromString "(%", pp reg, fromString ")"]

instance X86Print Ind where
  format (I bReg iReg) = mconcat [fromString "(%", pp bReg, fromString ", %", pp iReg, fromString ")"]

type OpCode = String

-- | Emit an opcode with no arguments
emitOp1 :: OpCode -> Gen
emitOp1 op = do gen "    "
                genLn op 

-- | Emit an opcode with one argument
emitOp2 :: (X86Print a) => OpCode -> a -> Gen
emitOp2 op a = do
  gen "    "
  gen op
  gen " "
  genLn a

-- | Emit an opcode with two arguments
emitOp3 :: (X86Print a, X86Print b) => OpCode -> a -> b -> Gen
emitOp3 op a b = do
  gen "    "
  gen op
  gen " "
  gen a
  gen ", "
  genLn b

-- | Emit a label from a the `Label' type
emitLabelLabel :: Label -> Gen
emitLabelLabel (L name ind) = do
  gen "L"
  gen $ pp ind
  genLn ":"

-- | Emit a label from a literal
emitLabel :: (X86Print a) => a -> Gen
emitLabel a = do
  gen a
  genLn ":"

-- | Emit an opcode with a label as its operand
emitJumpLabel :: OpCode -> Label -> Gen
emitJumpLabel op (L name ind) = emitOp2 op (fromString "L" `mappend` pp ind)

-- | Emit a jump opcode
emitJump :: (X86Print a) => OpCode -> a -> Gen
emitJump op a = emitOp2 op (fromString "*" `mappend` format a)

--emitOp1 :: Handle -> OpCode -> IO ()
--emitOp1 h op = hPutStrLn h ("    " ++ op)
--
--emitOp2 :: (X86Print a) => Handle -> OpCode -> a -> IO ()
--emitOp2 h op a = hPutStrLn h ("    " ++ op ++ " " ++ format a)
--
--emitOp3 :: (X86Print a, X86Print b) => Handle -> OpCode -> a -> b -> IO ()
--emitOp3 h op a b = hPutStrLn h ("    " ++ op ++ " " ++ format a ++ ", " ++ format b)
--
--emitLabel :: (X86Print a) => Handle -> a -> IO ()
--emitLabel h a = hPutStrLn h (format a ++ ":")
--
--emitJumpLabel :: Handle -> OpCode -> Label -> IO ()
--emitJumpLabel h op (L name ind) = emitOp2 h op ("L" ++ pp ind)
--
--emitJump :: (X86Print a) => Handle -> OpCode -> a -> IO ()
--emitJump = emitOp2


-- | Emit the pushq opcode
pushq :: (X86Print a) => a -> Gen
pushq = emitOp2 "pushq"

-- | Emit the popq opcode
popq :: (X86Print a) => a -> Gen
popq = emitOp2 "popq"

-- | Emit the movq opcode
movq :: (X86Print a, X86Print b) => a -> b -> Gen
movq = emitOp3 "movq"

-- | Emit the leaq opcode
leaq :: (X86Print a, X86Print b) => a -> b -> Gen
leaq = emitOp3 "leaq"

--pushq, popq :: (X86Print a) => Handle -> a -> IO ()
--movq, leaq :: (X86Print a, X86Print b) => Handle -> a -> b -> IO ()
--pushq h = emitOp2 h "pushq"
--popq h = emitOp2 h "popq"
--movq h = emitOp3 h "movq"
--leaq h = emitOp3 h "leaq"

-- | Emit the boilderplate code for entering the scheme runtime
emitEntry :: Gen
emitEntry = do
  c <- getConfig
  emitOp2 ".globl" "_scheme_entry"
  emitLabel "_scheme_entry"
  pushq RBX
  pushq RBP
  pushq R12
  pushq R13
  pushq R14
  pushq R15
  movq  RDI (framePointerRegister c)
  movq  RSI (allocationPointerRegister c)
  leaq  "_scheme_exit(%rip)" (returnAddressRegister c)

--emitEntry :: P423Config -> Handle -> IO ()
--emitEntry c h =
--  do emitOp2 h ".globl" "_scheme_entry"
--     emitLabel h "_scheme_entry"
--     pushq h RBX
--     pushq h RBP
--     pushq h R12
--     pushq h R13
--     pushq h R14
--     pushq h R15
--     movq  h RDI (framePointerRegister c)
--     movq  h RSI (allocationPointerRegister c)
--     leaq  h "_scheme_exit(%rip)" (returnAddressRegister c)

-- | Emit the boilerplate code for exiting the scheme runtime
emitExit :: Gen
emitExit = do
  emitLabel "_scheme_exit"
  c <- getConfig
  unless (returnValueRegister c == RAX)
         (movq (returnValueRegister c) RAX)
  popq R15
  popq R14
  popq R13
  popq R12
  popq RBP
  popq RBX
  emitOp1 "ret"

--emitExit :: P423Config -> Handle -> IO ()
--emitExit c h =
--  do emitLabel h "_scheme_exit"
--     unless (returnValueRegister c == RAX)
--            (movq h (returnValueRegister c) RAX)
--     popq h R15
--     popq h R14
--     popq h R13
--     popq h R12
--     popq h RBP
--     popq h RBX
--     emitOp1 h "ret"

------------------------------------------------------------
-- Pretty Printing -----------------------------------------

class PP a where
  -- | Print to a Scheme SExp representation.
  pp :: a -> Builder

  -- | Pretty print the same Scheme SExp representation:
  ppp :: a -> P.Doc
  ppp = P.text . unpack . BBB.toByteString . pp

-- | Build a list SExp 
ppSexp :: [Builder] -> Builder
ppSexp ls = fromString "(" `mappend` mconcat (intersperse (fromString " ") ls) `mappend` fromString ")"

-- | Build a multi-line pretty-printed SExp
-- pppSexp :: [P.Doc] -> P.Doc
-- pppSexp ls = P.parens$ P.sep ls

-- Getting a hang for keywords is a bit hard:
pppSexp :: [P.Doc] -> P.Doc
-- pppSexp [] = P.parens P.empty
-- pppSexp [a,b] = P.parens$ P.sep [a,b]
pppSexp (h1:h2:ls) | isSchemeKwd (P.render h1) = P.parens$ P.sep$ (h1 P.<+> h2):ls
--  | otherwise =
pppSexp ls = P.parens$ P.sep ls

isSchemeKwd :: String -> Bool
-- isSchemeKwd = all (\c -> isAlpha c || c=='-')
isSchemeKwd = flip elem ["locals","letrec","lambda","register-conflict"]


instance PP Builder where
  pp = id
  
instance PP ByteString where
  pp = fromByteString
  
instance PP String where
  pp = fromString
  ppp = P.text 

instance PP Bool where
  pp = fromString . show

instance PP Integer where
  pp = fromShow
  ppp = P.text . show

instance PP UVar where
  pp (UV name ind) = mconcat [fromString name, fromString ".",  fromShow ind]

instance PP FVar where
  pp (FV ind) = mconcat [fromString "fv", fromShow ind]

instance PP Label where
  pp (L name ind) = mconcat [fromString name, fromChar '$', fromShow ind]

instance PP Disp where
  pp (D r i) = ppSexp [fromString "disp", (pp r), (pp i)]

instance PP Ind where
  pp (I r1 r2) = ppSexp [fromString "index", (pp r1), (pp r2)]

instance PP Relop where
  pp r = case r of
    LT  -> fromString "<"
    LTE -> fromString "<="
    EQ  -> fromString "="
    GTE -> fromString ">="
    GT  -> fromString ">"

instance PP Binop where
  pp b = case b of
    MUL    -> fromString "*"
    ADD    -> fromString "+"
    SUB    -> fromString "-"
    LOGAND -> fromString "logand"
    LOGOR  -> fromString "logor"
    SRA    -> fromString "sra"

instance PP Reg where
  pp r = case r of
    RAX -> fromString "rax"
    RCX -> fromString "rcx"
    RDX -> fromString "rdx"
    RBX -> fromString "rbx"
    RBP -> fromString "rbp"
    RSI -> fromString "rsi"
    RDI -> fromString "rdi"
    R8  -> fromString "r8"
    R9  -> fromString "r9"
    R10 -> fromString "r10"
    R11 -> fromString "r11"
    R12 -> fromString "r12"
    R13 -> fromString "r13"
    R14 -> fromString "r14"
    R15 -> fromString "r15"

instance PP EffectPrim where
  pp b = case b of
    SetCar    -> fromString "set-car!"
    SetCdr    -> fromString "set-cdr!"
    VectorSet -> fromString "vector-set!"
    ProcedureSet -> fromString "procedure-set!"

instance PP PredPrim where
  pp p = fromString $ case p of
    Lt -> "<" ; Lte -> "<=" ; Eq -> "=" ; Gte -> ">=" ; Gt -> ">"
    BooleanP -> "boolean?" ; EqP -> "eq?" ; FixnumP -> "fixnum?"
    NullP -> "null?" ; PairP -> "pair?" ; VectorP -> "vector?"
    ProcedureP -> "procedure?"

instance PP ValPrim where
  pp p = fromString$ case p of    
    Times -> "*" ; Plus -> "+" ; Minus -> "-"; Car -> "car" ; Cdr -> "cdr" ; Cons -> "cons"
    MakeVector -> "make-vector" ; VectorLength -> "vector-length" ; VectorRef -> "vector-ref"
    Void -> "void"
    MakeProcedure -> "make-procedure" ; ProcedureCode -> "procedure-code" ; ProcedureRef -> "procedure-ref"

instance PP Immediate where
  pp p = fromString$ case p of
    Fixnum i -> show i
    NullList -> "()"
    HashT -> "#t"
    HashF -> "#f"

------------------------------------------------------------
-- Parsing -------------------------------------------------

-- | Throwing an error inside the "parser".
parseFailureM :: String -> PassM a
parseFailureM = lift . Left 
-- parseFailureM = return . Left . ParseErrorException

-- | Throwing an error inside the parser, non-monadic version.
-- parseFailure :: String -> a
-- parseFailure = throw . ParseErrorException


-- | Parse a number
parseSuffix :: String -> PassM Integer
parseSuffix i@('0':rest) =
  if (null rest)
     then return 0
     else parseFailureM ("parseSuffix: Leading zero in index: " ++ i)
parseSuffix i =
  if (and $ map isDigit i)
     then return $ read i
     else parseFailureM ("parseSuffix: Not a number: " ++ i)

parseListWithFinal :: (LispVal -> PassM a) -> (LispVal -> PassM b) ->
                        [LispVal] -> PassM ([a],b)
parseListWithFinal fa fb [] = parseFailureM ("parseListWithFinal: List must have at least one element")
parseListWithFinal fa fb [b] =
  do b <- fb b
     return ([],b)
parseListWithFinal fa fb (a:asb) =
  do a <- fa a
     (as,b) <- parseListWithFinal fa fb asb
     return (a:as,b)

parseUVar :: LispVal -> PassM UVar
parseUVar (Symbol s) = case (split '.' s) of
  (_,"")      -> parseFailureM ("parseUVar: No index: " ++ s)
  (name,ind)  -> do ind <- parseSuffix ind; return (UV name ind)
parseUVar e = parseFailureM ("parseUVar: Not a symbol: " ++ show e)

parseFVar :: LispVal -> PassM FVar
parseFVar (Symbol s) = case s of
  ('f':'v':ind) -> do ind <- parseSuffix ind; return (FV ind)
  _             -> parseFailureM ("parseFVar: Not a framevar: " ++ s)
parseFVar e = parseFailureM ("parseFVar: Not a symbol: " ++ show e)

parseLabel :: LispVal -> PassM Label
parseLabel (Symbol s) = case (split '$' s) of
  (_,"")     -> parseFailureM ("parseLabel: No index: " ++ s)
  (name,ind) -> do ind <- parseSuffix ind; return (L name ind)
parseLabel e = parseFailureM ("parseLabel: Not a symbol: " ++ show e)

-- parseLabel :: LispVal -> Exc Label
-- parseLabel (Symbol s) = case (split '$' s) of
--   (_,"")     -> failure ("No index: " ++ s)
--   (name,ind) -> do ind <- parseSuffix ind; return (L name ind)
-- parseLabel e = failure ("Not a symbol: " ++ show e)

parseRelop :: LispVal -> PassM Relop
parseRelop (Symbol s) = case s of
  "<"  -> return LT
  "<=" -> return LTE
  "="  -> return EQ
  ">=" -> return GTE
  ">"  -> return GT
  e    -> parseFailureM ("parseRelop: Not a relop: " ++ e)
parseRelop e = parseFailureM ("parseRelop: Not a symbol: " ++ show e)

parseBinop :: LispVal -> PassM Binop
parseBinop (Symbol s) = case s of
  "logand" -> return LOGAND
  "logor"  -> return LOGOR
  "sra"    -> return SRA
  "*"      -> return MUL
  "+"      -> return ADD
  "-"      -> return SUB
  e        -> parseFailureM ("parseBinop: Not a binop: " ++ e)
parseBinop e = parseFailureM ("parseBinop: Not a symbol: " ++ show e)

parseReg :: LispVal -> PassM Reg
parseReg (Symbol s) = case s of
  "rax" -> return RAX
  "rcx" -> return RCX
  "rdx" -> return RDX
  "rbp" -> return RBP
  "rbx" -> return RBX
  "rsi" -> return RSI
  "rdi" -> return RDI
  "r8"  -> return R8
  "r9"  -> return R9
  "r10" -> return R10
  "r11" -> return R11
  "r12" -> return R12
  "r13" -> return R13
  "r14" -> return R14
  "r15" -> return R15
  e     -> parseFailureM ("parseReg: Not a register: " ++ e)
parseReg e = parseFailureM ("parseReg: Not a symbol: " ++ show e)

parseInt32 :: LispVal -> PassM Integer
parseInt32 (IntNumber i) = if isInt32 n
                              then return n
                              else parseFailureM ("parseInt32: Out of range: " ++ show i)
  where n = fromIntegral i
parseInt32 e = parseFailureM ("parseInt32: Not an int: " ++ show e)

parseInt64 :: LispVal -> PassM Integer
parseInt64 (IntNumber i) = if isInt64 n
                              then return (fromIntegral n)
                              else parseFailureM ("parseInt64: Out of range: " ++ show i)
  where n = fromIntegral i
parseInt64 e = parseFailureM ("parseInt64: Not an int: " ++ show e)


-- TODO: Could use a single association list to go both directions:
parseValPrim :: LispVal -> PassM ValPrim
parseValPrim (Symbol s) = case s of
  "*"      -> return Times
  "+"      -> return Plus
  "-"      -> return Minus
  "car"    -> return Car
  "cdr"    -> return Cdr
  "cons"   -> return Cons
  "make-vector" -> return MakeVector
  "vector-length" -> return VectorLength
  "vector-ref"    -> return VectorRef
  "void"          -> return Void
  e        -> parseFailureM ("parseValPrim: Not a value primitive: " ++ e)
parseValPrim e = parseFailureM ("parseValPrim: Not a symbol: " ++ show e)

parsePredPrim :: LispVal -> PassM PredPrim
parsePredPrim (Symbol s) = case s of
  "<"      -> return Lt
  "<="     -> return Lte
  "="      -> return Eq
  ">="     -> return Gte
  ">"      -> return Gt
  "boolean?" -> return BooleanP
  "eq?"      -> return EqP
  "fixnum?"  -> return FixnumP
  "null?"    -> return NullP
  "pair?"    -> return PairP
  "vector?"  -> return VectorP     
  "procedure?"  -> return ProcedureP     
  e        -> parseFailureM ("parsePredPrim: Not a pred primitive: " ++ e)
parsePredPrim e = parseFailureM ("parsePredPrim: Not a symbol: " ++ show e)

parseEffectPrim :: LispVal -> PassM EffectPrim
parseEffectPrim (Symbol s) = case s of
  "set-car!" -> return SetCar
  "set-cdr!" -> return SetCdr
  "vector-set!"-> return VectorSet
  e        -> parseFailureM ("parseEffectPrim: Not an effect primitive: " ++ e)
parseEffectPrim e = parseFailureM ("parseEffectPrim: Not a symbol: " ++ show e)



------------------------------------------------------------
-- Parse Helpers -------------------------------------------

inBitRange :: (Integral a) => Integer -> a -> Bool
inBitRange r i = (((- (2 ^ (r-1))) <= n) && (n <= ((2 ^ (r-1)) - 1)))
  where n = fromIntegral i

isInt32 = inBitRange 32
isInt64 = inBitRange 64

isFixnum :: Integral a => a -> Bool
isFixnum = inBitRange fixnumBits

isUInt6 :: Integer -> Bool
isUInt6 i = (0 <= i) && (i <= 63)

class SuffixTerm a where
  extractSuffix :: a -> Integer
  uniqueSuffixes :: [a] -> Bool
  uniqueSuffixes as = isSet $ map extractSuffix as

isSet :: Ord a => [a] -> Bool
isSet ls = length ls == S.size (S.fromList ls)

instance SuffixTerm UVar where
  extractSuffix (UV name ind) = ind

instance SuffixTerm FVar where
  extractSuffix (FV ind) = ind

instance SuffixTerm Label where
  extractSuffix (L name ind) = ind

wordShift :: Integer
wordShift = 3

ash :: Integer -> Integer -> Integer
ash n = (* (2 ^ n))

-- | Remove whitespace from both ends of a string.
chomp :: String -> String
chomp = reverse . dropWhile isSpace . reverse


-- | Bit range of a valid boxed signed immediate integer
fixnumBits :: Integer
fixnumBits = 64 - (fromIntegral shiftFixnum)

maskFixnum :: Int64
maskFixnum = 0x7 -- 0b111

maskPair :: Int64
maskPair = 0x7 -- 0b111

maskVector :: Int64
maskVector = 0x7 -- 0b111

maskProcedure :: Int64
maskProcedure = 0x7 -- 0b111

maskBoolean :: Int64
maskBoolean = 0xF7 -- 0b11110111

-- | Left-shift for integer immediates
shiftFixnum :: Int
shiftFixnum = 3

-- | Tag for fixnum values
tagFixnum :: Integer
tagFixnum = 0x0

-- | Tag for pair values
tagPair :: Integer
tagPair = 0x1

-- | Tag for procedure values
tagProcedure :: Integer
tagProcedure = 0x2

tagVector :: Integer
tagVector = 0x3

tagBoolean :: Integer
tagBoolean = 0x6

tagNonfixnum :: Integer
tagNonfixnum = 0x6

repFalse :: Integer
repFalse = shiftL 0x0 shiftFixnum + tagNonfixnum

repTrue :: Integer
repTrue = shiftL 0x1 shiftFixnum + tagNonfixnum

repNil :: Integer
repNil = shiftL 0x2 shiftFixnum + tagNonfixnum

repVoid :: Integer
repVoid = shiftL 0x3 shiftFixnum + tagNonfixnum

dispCar :: Integer
dispCar = 0

dispCdr :: Integer
dispCdr = 8

sizePair :: Integer
sizePair = 2 * dispCdr

dispVectorLength :: Integer
dispVectorLength = 0

dispVectorData :: Integer
dispVectorData = 8

dispProcedureCode :: Integer
dispProcedureCode = 0

dispProcedureData :: Integer
dispProcedureData = 8

