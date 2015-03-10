module CompilerHs.Compile
  ( p423Compile
  ) where

import System.IO
import System.Cmd
import System.Process
import System.Exit
import Control.Exception (throw)

import FrameworkHs.Driver
import FrameworkHs.Prims
import FrameworkHs.Helpers
import FrameworkHs.SExpReader.LispData

import FrameworkHs.ParseL01                    (parseProg)

import CompilerHs.VerifyUIL                    (verifyUIL)
import CompilerHs.UncoverRegisterConflict      (uncoverRegisterConflict)
import CompilerHs.AssignRegisters              (assignRegisters)

import CompilerHs.UncoverFrameConflict         (uncoverFrameConflict)
import CompilerHs.SelectInstructions           (selectInstructions)
import CompilerHs.EverybodyHome                (everybodyHome)
import CompilerHs.AssignFrame                  (assignFrame)
import CompilerHs.PreAssignFrame               (preAssignFrame)
import CompilerHs.AssignNewFrame               (assignNewFrame)
import CompilerHs.FinalizeFrameLocations       (finalizeFrameLocations)

import CompilerHs.DiscardCallLive              (discardCallLive)
import CompilerHs.FinalizeLocations            (finalizeLocations)
import CompilerHs.ExposeBasicBlocks            (exposeBasicBlocks)
import CompilerHs.ExposeFrameVar               (exposeFrameVar)
import CompilerHs.FlattenProgram               (flattenProgram)
import CompilerHs.GenerateX86_64               (generateX86_64)

import CompilerHs.FlattenSet                   (flattenSet)
import CompilerHs.RemoveComplexOpera           (removeComplexOpera)
import CompilerHs.ImposeCallingConventions     (imposeCallingConventions)

import qualified Data.ByteString as B

vfs = P423Pass { pass = verifyUIL
               , passName = "verifyUIL"
               , wrapperName = "verify-uil/wrapper"
               , trace = False
               }

urc = P423Pass { pass = uncoverRegisterConflict
               , passName = "uncoverRegisterConflict"
               , wrapperName = "uncover-register-conflict/wrapper"
               , trace = False
               }

asr = P423Pass { pass = assignRegisters
               , passName = "assignRegisters"
               , wrapperName = "assign-registers/wrapper"
               , trace = False
               }

dcl = P423Pass { pass = discardCallLive
               , passName = "discardCallLive"
               , wrapperName = "discard-call-live/wrapper"
               , trace = False
               }

fnl = P423Pass { pass = finalizeLocations
               , passName = "finalizeLocations"
               , wrapperName = "finalize-locations/wrapper"
               , trace = False
               }

efv = P423Pass { pass = exposeFrameVar
               , passName = "exposeFrameVar"
               , wrapperName = "expose-frame-var/wrapper"
               , trace = False
               }
      
emo = P423Pass { pass = exposeMemoryOperands
               , passName = "exposeMemoryOperands"
               , wrapperName = "expose-memory-operands/wrapper"
               , trace = False
               }

ebb = P423Pass { pass = exposeBasicBlocks
               , passName = "exposeBasicBlocks"
               , wrapperName = "expose-basic-blocks/wrapper"
               , trace = False
               }

flp = P423Pass { pass = flattenProgram
               , passName = "flattenProgram"
               , wrapperName = "flatten-program/wrapper"
               , trace = False
               }

ufc = P423Pass { pass = uncoverFrameConflict
               , passName = "uncoverFrameConflict"
               , wrapperName = "uncover-frame-conflict/wrapper"
               , trace = False
               }

sis = P423Pass { pass = selectInstructions
               , passName = "selectInstructions"
               , wrapperName = "select-instructions/wrapper"
               , trace = False
               }

paf = P423Pass { pass = preAssignFrame
               , passName = "preAssignFrame"
               , wrapperName = "pre-assign-frame/wrapper"
               , trace = False
               }

anf = P423Pass { pass = assignNewFrame
               , passName = "assignNewFrame"
               , wrapperName = "assign-new-frame/wrapper"
               , trace = False
               }

asf = P423Pass { pass = assignFrame
               , passName = "assignFrame"
               , wrapperName = "assign-frame/wrapper"
               , trace = False
               }

ffl = P423Pass { pass = finalizeFrameLocations
               , passName = "finalizeFrameLocations"
               , wrapperName = "finalize-frame-locations/wrapper"
               , trace = False
               }

rco = P423Pass { pass = removeComplexOpera
               , passName = "removeComplexOpera"
               , wrapperName = "remove-complex-opera*/wrapper"
               , trace = False 
               }

fls = P423Pass { pass = flattenSet
               , passName = "flattenSet"
               , wrapperName = "flatten-set!/wrapper"
               , trace = False 
               }

icc = P423Pass { pass = imposeCallingConventions
               , passName = "imposeCallingConventions"
               , wrapperName = "impose-calling-conventions/wrapper"
               , trace = False 
               }

eap = P423Pass { pass = exposeAllocationPointer
               , passName = "exposeAllocationPointer"
               , wrapperName = "expose-allocation-pointer/wrapper"
               , trace = False 
               }

-- | Compose the complete compiler as a pipeline of passes.
p423Compile :: LispVal -> CompileM String
p423Compile l = do
  p <- liftPassM$ parseProg l  
  p <- runPass vfs p
  p <- runPass rco p
  p <- runPass fls p
  p <- runPass icc p
  p <- runPass ufc p
  p <- runPass paf p
  p <- runPass anf p
  let loop p = do 
        p <- runPass ffl p
        p <- runPass sis p
        p <- runPass urc p
        p <- runPass asr p
        let b = everybodyHome p
        if b then return p
         else do 
           p <- runPass asf p
           loop p
  p <- loop p
  p <- runPass dcl p
  p <- runPass fnl p
  p <- runPass efv p
  p <- runPass ebb p
  p <- runPass flp p
  assemble$ generateX86_64 p
