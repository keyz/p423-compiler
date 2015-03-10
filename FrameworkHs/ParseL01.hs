
module FrameworkHs.ParseL01 where

import Control.Applicative ((<$>))
import Debug.Trace         (trace)
import FrameworkHs.GenGrammars.L22VerifyUil
import FrameworkHs.SExpReader.LispData
import FrameworkHs.Prims

import FrameworkHs.Helpers (parseListWithFinal, parseInt32, parseInt64, parseLabel, parseUVar,
                            parseFVar, parseRelop, parseBinop, parseReg, parseFailureM, PassM, orPassM)

parseProg :: LispVal -> PassM Prog
parseProg (List [(Symbol "letrec"),List bs,b]) =
  do
     bs' <- mapM parseTuple bs
     b'  <- parseBody b
     return (Letrec bs' b')
  where parseTuple :: LispVal -> PassM (Label,[UVar],Body)
        parseTuple (List [l,List[(Symbol "lambda"),List args,b]]) =
          do 
             l' <- parseLabel l
             b' <- parseBody b
             args' <- mapM parseUVar args
             return (l',args',b')
        parseTuple e = parseFailureM ("parseProg: Invalid tuple: " ++ show e)
parseProg e = parseFailureM ("parseProg: Invalid Prog: " ++ show e)



parseBody :: LispVal -> PassM Body
parseBody (List [(Symbol "locals"),List bs,t]) =
  do
     us <- mapM parseUVar bs
     t  <- parseTail t
     return (Locals us t)
parseBody e = parseFailureM ("Invalid Body: " ++ show e)


parseTail :: LispVal -> PassM Tail
parseTail (List [(Symbol "if"),p,t1,t2]) =
  do p <- parsePred p
     t1 <- parseTail t1
     t2 <- parseTail t2
     return (IfT p t1 t2)
parseTail (List ((Symbol "begin"):ls)) =
  do (es,t) <- parseListWithFinal parseEffect parseTail ls
     return (BeginT es t)

parseTail (List [Symbol "alloc",a]) =
  do a' <- parseValue a
     return (AllocT a')
parseTail (List [Symbol "mref",a,b]) =
  do a' <- parseValue a
     b' <- parseValue b
     return (MrefT a' b')
     
parseTail (List (hd:ls)) =
  -- In the application case we need to enable BACKTRACKING:
  orPassM (do v  <- parseValue hd
              ls <- mapM parseValue ls
              return (AppT2 v ls))
          (do b  <- parseBinop hd
              ls <- mapM parseValue ls
              case ls of
                [x,y] -> return$ AppT1 b x y
                ls    -> parseFailureM$ "invalid binop args: "++show ls)
parseTail e =
  (TrivT <$> parseTriv e) `orPassM`
  (parseFailureM ("Invalid Tail: " ++ show e))


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

parseValue (List [Symbol "alloc",a]) =
  do a' <- parseValue a
     return (AllocV a')
parseValue (List [Symbol "mref",a,b]) =
  do a' <- parseValue a
     b' <- parseValue b
     return (MrefV a' b')
  
parseValue (List (op:rst)) = do
  firstItem <- orPassM (fmap Left $ parseBinop op) (fmap Right $ parseValue op)
  case firstItem of
    Left binop -> do
     rst'   <- mapM parseValue rst
     case rst' of
       [x,y] -> return $ AppV1 binop x y
       _     -> parseFailureM "parseValue: Wrong number of args to binop"
    Right val' -> do
      rst' <- mapM parseValue rst
      return $ AppV2 val' rst'
         



parseValue triv =
--  trace ("Is this triv?" ++show triv) $
  TrivV <$> parseTriv triv

parsePred :: LispVal -> PassM Pred
parsePred (List [(Symbol "true")]) = return (TrueP)
parsePred (List [(Symbol "false")]) = return (FalseP)
parsePred (List ((Symbol "begin"):ls)) =
  do (es,p) <- parseListWithFinal parseEffect parsePred ls
     return (BeginP es p)
parsePred (List [r,v1,v2]) =
  do r  <- parseRelop r
     v1 <- parseValue v1
     v2 <- parseValue v2
     return (AppP r v1 v2)
parsePred (List [(Symbol "if"),p1,p2,p3]) =
  do p1 <- parsePred p1
     p2 <- parsePred p2
     p3 <- parsePred p3
     return (IfP p1 p2 p3)
parsePred e = parseFailureM ("Invalid Pred: " ++ show e)


parseEffect :: LispVal -> PassM Effect
parseEffect (List [(Symbol "nop")]) = return (Nop)
parseEffect (List [(Symbol "set!"),v,rhs]) =
  do v   <- parseUVar v
     rhs <- parseValue rhs
     return (Set v rhs)
parseEffect (List [(Symbol "mset!"),v,ix,rhs]) =
  do v'   <- parseValue v
     ix'  <- parseValue ix
     rhs' <- parseValue rhs
     return (Mset v' ix' rhs')
parseEffect (List [(Symbol "if"),p,e1,e2]) =
  do p <- parsePred p
     e1 <- parseEffect e1
     e2 <- parseEffect e2
     return (IfE p e1 e2)
parseEffect (List ((Symbol "begin"):ls)) =
  do (es,e) <- parseListWithFinal parseEffect parseEffect ls
     return (BeginE es e)
parseEffect (List (fn:ls)) =
  do v <- parseValue fn
     vs <- mapM parseValue ls
     return $ AppE v vs
parseEffect e = parseFailureM ("Invalid Effect: " ++ show e)


parseTriv :: LispVal -> PassM Triv
parseTriv i@(IntNumber _) =
  do i <- parseInt64 i
     return (Integer i)
parseTriv s@(Symbol _) =
  (UVar  <$> parseUVar s)   `orPassM`
  (Label <$> parseLabel s) 
parseTriv e = parseFailureM ("Invalid Triv: " ++ show e)

