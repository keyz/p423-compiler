
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
parseProg (List [(Symbol "letrec"),List bs,b]) =
  do
     bs' <- mapM parseTuple bs
     b'  <- parseValue b
     return (Letrec bs' b')
  where parseTuple :: LispVal -> PassM (Label,[UVar],Value)
        parseTuple (List [l,List[(Symbol "lambda"),List args,b]]) =
          do 
             l' <- parseLabel l
             b' <- parseValue b
             args' <- mapM parseUVar args
             return (l',args',b')
        parseTuple e = parseFailureM ("parseProg: Invalid tuple: " ++ show e)
parseProg e = parseFailureM ("parseProg: Invalid Prog: " ++ show e)

parseBinds :: LispVal -> PassM [(UVar,Value)]
parseBinds (List ls) = mapM fn ls
 where
   fn (List [lhs,rhs]) = 
     do uv   <- parseUVar lhs
        rhs' <- parseValue rhs
        return (uv,rhs')

parseValue :: LispVal -> PassM Value
parseValue (List [Symbol "begin"]) = parseFailureM "empty begin form"
parseValue (List (Symbol "begin" : rst)) = do  
  es' <- mapM parseEffect (init rst)
  v'  <- parseValue (last rst)
  return$ BeginV es' v'
parseValue (List [Symbol "if",p,v1,v2]) = do
  p'  <- parsePred p
  v1' <- parseValue v1
  v2' <- parseValue v2
  return$ IfV p' v1' v2'
parseValue (List [(Symbol "let"),binds,bod]) =
  do binds' <- parseBinds binds
     bod'   <- parseValue bod
     return (LetV binds' bod')
parseValue (List [Symbol "quote",imm]) =
  Quote <$> parseImmediate imm     
parseValue (List (op:rst)) = do
  firstItem <- orPassM (fmap Left $ parseValPrim op) (fmap Right $ parseValue op)
  case firstItem of
    Left op -> do
     rst'   <- mapM parseValue rst
     return $ AppV1 op rst'
    Right val' -> do
      rst' <- mapM parseValue rst
      return $ AppV2 val' rst'
parseValue sym@(Symbol _) =
  (UVar  <$> parseUVar sym)   `orPassM`
  (Label <$> parseLabel sym) 

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
  

parsePred :: LispVal -> PassM Pred
parsePred (List [(Symbol "true")]) = return (TrueP)
parsePred (List [(Symbol "false")]) = return (FalseP)
parsePred (List ((Symbol "begin"):ls)) =
  do (es,p) <- parseListWithFinal parseEffect parsePred ls
     return (BeginP es p)
parsePred (List [(Symbol "if"),p1,p2,p3]) =
  do p1 <- parsePred p1
     p2 <- parsePred p2
     p3 <- parsePred p3
     return (IfP p1 p2 p3)
parsePred (List [(Symbol "let"),binds,bod]) =
  do binds' <- parseBinds binds
     bod'   <- parsePred bod
     return (LetP binds' bod')
parsePred (List (r : rest)) =
  do r     <- parsePredPrim r
     rest' <- mapM parseValue rest
     return (AppP r rest')
parsePred e = parseFailureM ("Invalid Pred: " ++ show e)


parseEffect :: LispVal -> PassM Effect
parseEffect (List [(Symbol "nop")]) = return (Nop)
parseEffect (List [(Symbol "if"),p,e1,e2]) =
  do p <- parsePred p
     e1 <- parseEffect e1
     e2 <- parseEffect e2
     return (IfE p e1 e2)
parseEffect (List [(Symbol "let"),binds,bod]) =
  do binds' <- parseBinds binds
     bod'   <- parseEffect bod
     return (LetE binds' bod')
parseEffect (List ((Symbol "begin"):ls)) =
  do (es,e) <- parseListWithFinal parseEffect parseEffect ls
     return (BeginE es e)

parseEffect (List (op:rst)) = do
  firstItem <- orPassM (fmap Left $ parseEffectPrim op) (fmap Right $ parseValue op)
  case firstItem of
    Left op -> do
     rst'   <- mapM parseValue rst
     return$ AppE1 op rst'
    Right val' -> do
      rst' <- mapM parseValue rst
      return $ AppE2 val' rst'
     
parseEffect e = parseFailureM ("Invalid Effect: " ++ show e)


