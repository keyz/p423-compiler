
module FrameworkHs.ParseL01 where

import Control.Applicative ((<$>))
import Debug.Trace         (trace)
import FrameworkHs.GenGrammars.L01VerifyScheme
import FrameworkHs.SExpReader.LispData
import FrameworkHs.Prims
import FrameworkHs.Helpers (parseListWithFinal, parseInt32, parseInt64, parseLabel, parseUVar,
                            parseFailureM, PassM, orPassM, isInt64, 
                            parseValPrim, parseEffectPrim, parsePredPrim)

parseProg :: LispVal -> PassM Prog
parseProg l = Expr <$> parseExpr l

parseBinds :: LispVal -> PassM [(UVar,Expr)]
parseBinds (List ls) = mapM fn ls
 where
   fn (List [lhs,rhs]) = 
     do uv   <- parseUVar lhs
        rhs' <- parseExpr rhs
        return (uv,rhs')

parseLetrecBinds :: LispVal -> PassM [(UVar, [UVar], Expr)]
parseLetrecBinds (List ls) = mapM fn ls
  where 
    fn (List [label,List [Symbol "lambda", List formals, bod]]) = do
      label' <- parseUVar label
      formals' <- mapM parseUVar formals
      bod' <- parseExpr bod
      return (label', formals', bod')
    

parseExpr :: LispVal -> PassM Expr
parseExpr (List [Symbol "begin"]) = parseFailureM "empty begin form"
parseExpr (List (Symbol "begin" : rst)) = do  
  es' <- mapM parseExpr (init rst)
  v'  <- parseExpr (last rst)
  return$ Begin es' v'
parseExpr (List [Symbol "letrec",binds,bod]) = do
  binds' <- parseLetrecBinds binds
  bod' <- parseExpr bod
  return $ Letrec binds' bod'
  

parseExpr (List [Symbol "if",p,v1,v2]) = do
  p'  <- parseExpr p
  v1' <- parseExpr v1
  v2' <- parseExpr v2
  return$ If p' v1' v2'

parseExpr (List [(Symbol "let"),binds,bod]) = do
  binds' <- parseBinds binds
  bod'   <- parseExpr bod
  return (Let binds' bod')

parseExpr (List [Symbol "quote",imm]) =
  Quote <$> parseImmediate imm   
  
parseExpr (List (op:rst)) = do
  firstItem <- (fmap App1 $ parseValPrim op) `orPassM` 
               (fmap App2 $ parseEffectPrim op) `orPassM`
               (fmap App3 $ parsePredPrim op) `orPassM`
               (fmap App4 $ parseExpr op)
  exprs <- mapM parseExpr rst
  return $ firstItem exprs

parseExpr sym@(Symbol _) =
  (UVar  <$> parseUVar sym) -- `orPassM`
--  (Label <$> parseLabel sym)

parseImmediate :: LispVal -> PassM Immediate
parseImmediate x =
  case x of
    IntNumber i | isInt64 i -> return$ Fixnum (fromInteger i)
                | otherwise -> parseFailureM
                               ("Invalid Immediate/fixnum: won't fit in Int64 " ++ show i)
    List []       -> return NullList
    Boolean True  -> return HashT
    Boolean False -> return HashF
    _ -> error$"TODO: parseImmediate: "++show x

