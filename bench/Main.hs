{-# OPTIONS_GHC -fno-cse -fno-ignore-asserts #-}
module Main
  ( main -- :: IO ()
  ) where
import           System.Environment

-- Import our benchmark suites
import qualified Misc      as Misc
import qualified Macro     as Macro
import qualified Micro     as Micro
import qualified Instances as Inst

-- A simple driver, for running every set of benchmarks.
main :: IO ()
main = do
  let badArgs = "invalid arguments; try 'misc', "
             ++ "'instances', 'macrobench' or 'microbench'"
  args <- getArgs
  case args of
    ("misc":args')       -> withArgs args' Misc.benchmarks
    ("instances":args')  -> withArgs args' Inst.benchmarks
    ("macrobench":args') -> withArgs args' Macro.benchmarks
    ("microbench":args') -> withArgs args' Micro.benchmarks
    []                   -> fail badArgs
    _                    -> fail badArgs
