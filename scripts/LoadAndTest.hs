import FrameworkHs.Testing
import FrameworkHs.Helpers (Option (Default, Option))

import System.Exit         (exitSuccess, exitFailure)
import System.Console.GetOpt as G
import System.Environment  (getArgs)

main = do
  argv <- getArgs
  let header = "Usage: LoadAndTest [OPTION...] [test-index ...]"
  (vs,ivs) <- case getOpt Permute options argv of
                (opts,other,[]  ) ->
                  case opts of
                    [] -> runDefault
                    [Invalid] -> runInvalid (map read other)
                    [Valid]   -> runValid   (map read other)
                    _ -> error "Cannot pass both --valid and --invalid"
                (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  
  if (all isPass vs && all isFail ivs)
   then exitSuccess
   else exitFailure



isPass :: TestResult -> Bool
isPass (Pass s) = True
isPass (Fail e) = False

isFail :: TestResult -> Bool
isFail (Fail e) = True
isFail (Pass s) = False

data Flag = Valid | Invalid
   deriving Show

options :: [OptDescr Flag]
options =
 [ G.Option ['v'] ["valid"]    (NoArg Valid)    "run a selection of valid tests (by index)"
 , G.Option ['i'] ["invalid"]  (NoArg Invalid)  "run a selection of invalid tests (by index)"
 ]

