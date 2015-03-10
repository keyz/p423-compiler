module FrameworkHs.Testing
  ( TestResult(..)
  , runDefault
  , runTestFile
  , defaultTestFile
  , defaultP423Config
  , runValid, runInvalid
  ) where

import Control.Exception (SomeException, handle, throw)
import Text.Printf

import FrameworkHs.SExpReader.Parser
import FrameworkHs.SExpReader.LispData
import FrameworkHs.Helpers
import FrameworkHs.Driver as D
import FrameworkHs.Prims
import CompilerHs.Compile

data Tests = Tests { valid :: [LispVal]
                   , invalid :: [LispVal]
                   } deriving (Show)

data TestResult = Pass String | Fail P423Exception
instance Show TestResult where
  show (Pass s) = s
  show (Fail e) = show e

-- | Select a set of tests to run:
data RunTests
  = AllFrom String
  | SelV [Int] RunTests  -- ^ A list of indices into valid tests
  | SelI [Int] RunTests  -- ^ A list of indices into invalid tests

defaultTestFile :: RunTests
defaultTestFile = AllFrom "test-suite.ss"

defaultP423Config :: P423Config
defaultP423Config = P423Config
         { framePointerRegister      = RBP
         , allocationPointerRegister = RDX
         , returnAddressRegister     = R15
         , returnValueRegister       = RAX
         , parameterRegisters        = [R8,R9]
         , runWrappers               = True
         }

catchTestFailures :: P423Exception -> Maybe P423Exception
catchTestFailures e = case e of
  (AssemblyFailedException _)   -> yes
  (ASTParseException _)         -> yes
  (ParseErrorException _)       -> no
  (NoValidTestsException)       -> no
  (NoInvalidTestsException)     -> no
  (PassFailureException _ _)    -> yes
  (WrapperFailureException _ _) -> yes
  where yes = Just e
        no  = Nothing

-- | Run the default set of tests (all found in the file).
--   Returns the test results for (valid,invalid) tests respectively.
runDefault :: IO ([TestResult],[TestResult])
runDefault = runTestsInternal defaultTestFile defaultP423Config

runInvalid :: [Int] -> IO ([TestResult],[TestResult])
runInvalid ixs = runTestsInternal (SelI ixs$ SelV [] defaultTestFile) defaultP423Config

runValid :: [Int] -> IO ([TestResult],[TestResult])
runValid ixs = runTestsInternal (SelI []$ SelV ixs defaultTestFile) defaultP423Config

showResults :: [TestResult] -> IO ()
showResults = mapM_ print

-- | Run all test cases contained in a file.  Returns the test results
-- for (valid,invalid) tests respectively.
runTestFile :: String -> P423Config -> IO ([TestResult],[TestResult])
runTestFile file conf = runTestsInternal (AllFrom file) conf

runTestsInternal :: RunTests -> P423Config -> IO ([TestResult],[TestResult])
runTestsInternal tests conf = runCompiler conf $ do
  ts <- lift$ getTests tests
  vs <- runSet "Valid"   p423Compile (valid ts)
  is <- runSet "Invalid" p423Compile (invalid ts)
  lift$ testResults vs is
  return (vs,is)

-- | Read tests from a test file.
getTests :: RunTests -> IO Tests
getTests t =
  case t of
    AllFrom f  -> lexTests f
    SelV vs t' -> do
      ts <- getTests t'
      let vs' = filterInd vs (valid ts)
      return Tests { valid = vs', invalid = invalid ts }
    SelI is t' -> do
      ts <- getTests t'
      let is' = filterInd is (invalid ts)
      return Tests { valid = valid ts, invalid = is' }

-- | Filter a list to only elements at the given set of positions.
filterInd :: [Int] -> [a] -> [a]
filterInd xs as =
  -- RRN: FIXME: this is quadratic:
  case xs of
    []                              -> []
    (x:xs')
      | x `elem` xs'                -> filterInd xs' as
      | (x >= 0) && (x < length as) -> (as !! x) : filterInd xs' as
      | otherwise                   -> filterInd xs' as

-- | Run a list of tests with a particular compiler.
runSet :: String -> (LispVal -> CompileM String) -> [LispVal] -> CompileM [TestResult]
runSet _ _ [] = return []
runSet setname compiler ts =
  do lift$ putStrLn ("\nTesting " ++ setname)
     lift$ putStrLn "Test    Result"
     lift$ putStrLn "---------------------------"
     mapIndexed 0 ts
  where mapIndexed i []     = return []
        mapIndexed i (t:ts) = do a <- wrapTest i t
                                 as <- mapIndexed (i+1) ts
                                 return (a:as)
        wrapTest :: Int -> LispVal -> CompileM TestResult
        wrapTest i l = D.catch 
                  (D.catch (do resetLastResult
                               res <- compiler l
                               lift$ printf "%4d    Pass\n" i
                               return $ Pass res)
                           (\e ->case catchTestFailures e of
                                  Nothing -> throw e 
                                  Just _ -> do
                                    printf "%4d    Fail    %s\n" i (shortExcDescrip e)
                                    return $ Fail e))
                  (\ e -> do let str = show (e :: SomeException)
                             printf "%4d    Fail    Error: %s\n" i str
                             return$ Fail (PassFailureException "" str))
        
testResults :: [TestResult] -> [TestResult] -> IO ()
testResults vs is =
  do putStrLn "\nTesting Summary"
     putStrLn "---------------------------"
     printf "%-24s%4d\n" "Expected Passes:" ep
     printf "%-24s%4d\n" "Unexpected Passes:" up
     printf "%-24s%4d\n" "Expected Failures:" ef
     printf "%-24s%4d\n" "Unexpected Failures:" uf
     printf "%-24s%4d\n\n" "Total:" t
  where ep = countPasses vs
        up = countPasses is
        ef = countFailures is
        uf = countFailures vs
        t  = length vs + length is

countFailures :: [TestResult] -> Int
countFailures = count isFail

countPasses :: [TestResult] -> Int
countPasses = count isPass

isPass :: TestResult -> Bool
isPass (Pass s) = True
isPass (Fail e) = False

isFail :: TestResult -> Bool
isFail (Fail e) = True
isFail (Pass s) = False

count :: (a -> Bool) -> [a] -> Int
count f [] = 0
count f (a:as)
  | f a = 1 + (count f as)
  | otherwise = count f as

lexTests :: String -> IO Tests
lexTests testFile =
  do mls <- lexFile testFile
     case mls of
       Left (Parser pe) -> throw $ ParseErrorException pe
       Right ls -> return $ findTests ls


handleTestFailure :: Int -> P423Exception -> IO TestResult
handleTestFailure i e = do putStrLn (show i ++ " : fail")
                           return $ Fail e

findTests :: [LispVal] -> Tests
findTests l = Tests {valid=findValid l,invalid=findInvalid l}
--findTests [(List l)] = Tests {valid=findValid l,invalid=findInvalid l}

findValid :: [LispVal] -> [LispVal]
findValid [] = throw NoValidTestsException
findValid (t:ts) = case t of
  List (Symbol "valid" : ls) -> ls
  _ -> findValid ts

findInvalid :: [LispVal] -> [LispVal]
findInvalid [] = throw NoInvalidTestsException
findInvalid (t:ts) = case t of
  List (Symbol "invalid" : ls) -> ls
  _ -> findInvalid ts
