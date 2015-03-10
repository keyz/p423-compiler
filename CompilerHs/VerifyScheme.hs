module CompilerHs.VerifyScheme (verifyScheme) where

import FrameworkHs.GenGrammars.L01VerifyScheme

import FrameworkHs.Prims
import FrameworkHs.Helpers

type Env = [UVar]

-- | Verify that the program is valid.  We try to catch as many errors
-- as possible in this pass, so that we do not encounter errors from
-- the middle of the compiler.
verifyScheme :: P423Config -> Prog -> Prog
verifyScheme c p@(Letrec ls b) = runPassM c $ do
  allDistinct "label" labels
  mapM_ (vBody labels) bodies
  vBody labels b
  return p
  where
    (labels,bodies) = unzip ls

--------------------------------------------------------------------------------
-- Helpers:

vBody :: [Label] -> Body -> PassM ()
vBody labels bo@(Locals uvars t) = do
  allDistinct "uvar" uvars
  vTail labels uvars t

vTail :: [Label] -> Env -> Tail -> PassM ()
vTail labels env ta = case ta of
  AppT tr _live -> vTriv labels env tr
  IfT p t1 t2 -> do vPred labels env p
                    vTail labels env t1
                    vTail labels env t2
  BeginT es t -> do mapM_ (vEffect labels env) es
                    vTail labels env t

vPred :: [Label] -> Env -> Pred -> PassM ()
vPred labels env pr = case pr of
  TrueP        -> return ()
  FalseP       -> return ()
  AppP r t1 t2 -> do vTriv labels env t1
                     vTriv labels env t2
  IfP p1 p2 p3 -> mapM_ (vPred labels env) [p1,p2,p3]
  BeginP es p  -> do mapM_ (vEffect labels env) es
                     vPred labels env p

relopConstraints :: Env -> Triv -> Triv -> Bool
relopConstraints env t1 t2 =
  (or [ (and [ (trivIsReg t1 env)
             , (or [ (trivIsReg t2 env)
                   , (trivIsFVar t2 env)
                   , (trivIsInt32 t2)
                   ])])
      , (and [ (trivIsFVar t1 env)
             , (or [ (trivIsReg t2 env)
                   , (trivIsInt32 t2)
                   ])])])

vEffect :: [Label] -> Env -> Effect -> PassM ()
vEffect labels env ef = case ef of
  Nop              -> return ()
  Set1 v tr        -> do vVar env v
                         vTriv labels env tr
  Set2 v b tr1 tr2 -> do vVar env v
                         assert (set2Constraints env b tr1 tr2) (constraintError ef)
  IfE p e1 e2      -> do vPred labels env p
                         vEffect labels env e1
                         vEffect labels env e2
  BeginE es e      -> do mapM_ (vEffect labels env) es
                         vEffect labels env e

set1Constraints :: Env -> Var -> Triv -> Bool
set1Constraints env v tr =
  (or [ (and [ (varIsReg v env)
             , (or [ (trivIsReg tr env)
                   , (trivIsFVar tr env)
                   , (trivIsInt64 tr)
                   , (trivIsLabel tr)
                   ])
             ])
      , (and [ (varIsFVar v env)
             , (or [ (trivIsReg tr env)
                   , (trivIsInt32 tr)
                   ])
             ])
      ])

set2Constraints :: Env -> Binop -> Triv -> Triv -> Bool
set2Constraints env b t1 t2 = case b of
  SRA -> trivIsUInt6 t2
  _   -> True

vTriv :: [Label] -> Env -> Triv -> PassM ()
vTriv labels env tr = case tr of
  Var v     -> vVar env v
  Integer i -> return ()
  Label l   -> assert (l `elem` labels) ("unbound label: " ++ show l)

vVar :: Env -> Var -> PassM ()
vVar env v = case v of
  UVar uv -> (assert (elem uv env) ("unbound uvar: " ++ show uv))
  Loc l   -> return ()

identicalError :: Var -> Triv -> Effect -> String
identicalError v tr e = show v ++ " and " ++ show tr ++ " must be identical in expression (" ++ show e ++ ")"

constraintError :: Show a => a -> String
constraintError x = show x ++ " violates machine constraints"

isJust :: Maybe a -> Bool
isJust x = case x of
  Just _  -> True
  Nothing -> False

assert :: Bool -> String -> PassM ()
assert False msg = passFailure "VerifyScheme" msg
assert True _ = return ()

allDistinct :: (LooseEq a, Eq a, Show a) => String -> [a] -> PassM ()
allDistinct name xs = case xs of
  []                     -> return ()
  [x]                    -> return ()
  (x:xs') | x `looseElem` xs' -> passFailure "VerifyScheme" ("duplicate " ++ name ++ ": " ++ show x)
          | otherwise    -> allDistinct name xs'

looseElem :: (LooseEq a) => a -> [a] -> Bool
looseElem e []     = False
looseElem e (x:xs) | e .= x = True
looseElem e (x:xs) = looseElem e xs

varIsReg :: Var -> Env -> Bool
varIsReg v env = case v of
  UVar uv     -> elem uv env
  Loc (Reg _) -> True
  _           -> False

varIsFVar :: Var -> Env -> Bool
varIsFVar v env = case v of
  UVar uv      -> elem uv env
  Loc (FVar _) -> True
  _            -> False

trivIsReg :: Triv -> Env -> Bool
trivIsReg tr env = case tr of
  Var v -> varIsReg v env
  _     -> False

trivIsFVar :: Triv -> Env -> Bool
trivIsFVar tr env = case tr of
  Var v -> varIsFVar v env
  _     -> False

trivIsInt32 :: Triv -> Bool
trivIsInt32 tr = case tr of
  Integer i -> isInt32 i
  _         -> False

trivIsLabel :: Triv -> Bool
trivIsLabel tr = case tr of
  Label _ -> True
  _       -> False

trivIsInt64 :: Triv -> Bool
trivIsInt64 tr = case tr of
  Integer i -> isInt64 i
  _         -> False

trivIsUInt6 :: Triv -> Bool
trivIsUInt6 tr = case tr of
  Integer i -> isUInt6 i
  _         -> False
