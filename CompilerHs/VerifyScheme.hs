module CompilerHs.VerifyScheme (verifyScheme) where

import FrameworkHs.GenGrammars.L01VerifyScheme

import Control.Arrow (first, second)
import Control.Monad.Reader
import Control.Monad.Writer

import FrameworkHs.Prims
import FrameworkHs.Helpers

type Env = [UVar]

-- | Verify that the program is valid.  We try to catch as many errors
-- as possible in this pass, so that we do not encounter errors from
-- the middle of the compiler.
verifyScheme :: P423Config -> Prog -> Prog
verifyScheme c p@(Expr e) = runPassM c $ do
  (((), uvs), labels) <- runWriterT $ runWriterT $ flip runReaderT ([], []) $ vExpr e

  allDistinct "uvar" uvs
  allDistinct "label" labels

  return p

--------------------------------------------------------------------------------
-- Helpers:

type VerifyM = ReaderT ([UVar], [Label])
               (WriterT [UVar]
               (WriterT [Label] PassM))


failure = passFailureM "verifyScheme"

verifyFailure = lift . lift . lift . failure

vExpr :: Expr -> VerifyM ()
vExpr e = case e of
  Quote imm -> case imm of
    Fixnum i -> flip assert (show i ++ " is out of range.") $ isFixnum $ fromIntegral i
    _ -> return ()
  Let binds e -> vLetBinds binds $ vExpr e
  Letrec binds e -> vLetrecBinds binds $ vExpr e
  If p c a -> vExpr p >> vExpr c >> vExpr a
  Begin effs e -> mapM vExpr effs >> vExpr e
  App1 vprim args -> let argsl = length args
                         arity = valPrimArity vprim
                     in flip assert ("Wrong number of args to primitive " ++ 
                                     show vprim ++ ". Expects " ++ show arity ++
                                     ", given " ++ show argsl ++ ".")
                                     $ argsl == arity
    
  App2 eprim args -> let argsl = length args
                         arity = effectPrimArity eprim
                     in flip assert ("Wrong number of args to primitive " ++ 
                                    show eprim ++ ". Expects " ++ show arity ++
                                    ", given " ++ show argsl ++ ".")
                                   $ argsl == arity
  App3 pprim args -> let argsl = length args
                         arity = predPrimArity pprim
                     in flip assert ("Wrong number of args to primitive " ++ 
                                     show pprim ++ ". Expects " ++ show arity ++
                                     ", given " ++ show argsl ++ ".")
                                    $ argsl == arity
  App4 fun args -> vExpr fun >> mapM_ vExpr args
  UVar uv -> asks (elem uv . fst) >>= flip assert ("Not in scope: " ++ show uv)
  Label label -> asks (elem label . snd) >>= flip assert ("Not in scope: " ++ show label)

-- A uvar is visible only in the bindings which follow it, and the tail of the
-- let
vLetBinds :: [(UVar, Expr)] -> VerifyM a -> VerifyM a
vLetBinds [] m = m
vLetBinds ((uv, expr):rest) m = do
  lift $ tell [uv]
  vExpr expr
  local (first (uv :)) $ do
    vLetBinds rest m

-- A label is visible through the whole letrec
vLetrecBinds :: [(Label, [UVar], Expr)] -> VerifyM a -> VerifyM a
vLetrecBinds binds m = do
  let (labels, formals, exprs) = unzip3 binds
      lambdas = zip formals exprs
  lift $ lift $ tell labels
  local (second (labels ++)) $ do
    mapM_ vLambda lambdas
    m

vLambda :: ([UVar], Expr) -> VerifyM ()
vLambda (formals, e) = do
  tell formals
  local (first $ const formals) $ vExpr e

assert :: Bool -> String -> VerifyM ()
assert False msg = verifyFailure msg
assert True _ = return ()

allDistinct :: (LooseEq a, Eq a, Show a) => String -> [a] -> PassM ()
allDistinct name xs = case xs of
  []                     -> return ()
  [x]                    -> return ()
  (x:xs')                -> 
    if x `looseElem` xs' 
    then failure ("duplicate " ++ name ++ ": " ++ show x)
    else allDistinct name xs'

looseElem :: (LooseEq a) => a -> [a] -> Bool
looseElem e []     = False
looseElem e (x:xs) | e .= x = True
looseElem e (x:xs) = looseElem e xs


