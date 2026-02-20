-- | Create a chain based on given arguments and run it.
--
-- Overall development policy:
--
-- * If you seek /elegant/ abstraction, you will get /elephant/ abstraction.
--
-- * All intermediate-languages should be interpretable in 'IO' monad with exactly same behavior,
--   or at least have such semantics.
--
-- * Interpreters should not try to optimize, use simplest implementation while keeping the order low.
--
-- See the source of 'help' for detailed description\/specification of features.
module Main where

import Control.Monad
import HS2BF
import qualified HS2BF.Brainfuck as Brainfuck
import qualified HS2BF.Core as Core
import qualified HS2BF.Front as Front
import qualified HS2BF.GMachine as GMachine
import qualified HS2BF.SAM as SAM
import HS2BF.Util
import qualified Paths_hs2bf
import System.Environment
import System.FilePath.Posix
import System.IO

main = execCommand =<< liftM parseArgs getArgs

-- | Parse arguments to 'Command'. Note this is a total function.
parseArgs :: [String] -> Command
parseArgs [] = ShowMessage $ version ++ "\n" ++ help
parseArgs ("-v" : _) = ShowMessage version
parseArgs ("--version" : _) = ShowMessage version
parseArgs ("-h" : _) = ShowMessage $ version ++ "\n" ++ help
parseArgs ("--help" : _) = ShowMessage $ version ++ "\n" ++ help
parseArgs ("--run" : n : as) = Interpret (parseOption as) n
parseArgs ("--make" : n : as) = Compile (parseOption as) n
parseArgs _ = ShowMessage "Invalid command. See 'hs2bf --help' for usage."

parseOption :: [String] -> Option
parseOption [] = Option {addrSpace = 2, verbose = True, debug = False, tolang = LangBF}
parseOption (term : xs) = case term of
  '-' : 'S' : 'c' : xs -> o {tolang = LangCore xs}
  '-' : 'S' : 'g' : xs -> o {tolang = LangGM xs}
  '-' : 'S' : 's' : xs -> o {tolang = LangSAM xs}
  "-Sb" -> o {tolang = LangBF}
  _ -> error $ "unknown option:" ++ term
  where
    o = parseOption xs

version :: String
version = "Haskell to Brainfuck Compiler: version 0.6.2"

help :: String
help =
  unlines $
    [ "Usage: hs2bf <command>",
      "",
      "command:",
      "  --help: show help",
      "  --version: show version",
      "  --run <module> <option>*: interpret <module>",
      "  --make <module> <option>*: compile <module>",
      "",
      "option:",
      "  -o <file> : output path (stdout if omitted)",
      "  -Sc : to Core code",
      "  -Scs: to Core code (simplified)",
      "  -Sg : to GMachine",
      "  -Sgr: to GMachine (simplified)",
      "  -Ss : to SAM",
      "  -Ssf: to SAM (most simplified)",
      --    ,"  -Sr : to SCGR" -- not implemented
      "  -Sb : to BF",
      "  --addr n : use n byte for pointer arithmetic",
      "  --debug : include detailed error message (this will make the program a LOT larger)",
      "",
      "examples:",
      "  hs2bf --make path/to/App.hs -o app : compile App.hs to bf",
      "  hs2bf --run Main -Sm : compile module Main to GMachine code and interpret it"
    ]
