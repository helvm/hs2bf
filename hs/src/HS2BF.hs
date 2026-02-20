module HS2BF where

import qualified HS2BF.Brainfuck as Brainfuck
import Control.Monad
import qualified HS2BF.Core as Core
import qualified HS2BF.Front as Front
import qualified HS2BF.GMachine as GMachine
import qualified Paths_hs2bf
import qualified HS2BF.SAM as SAM
import System.Environment
import System.FilePath.Posix
import System.IO
import HS2BF.Util

-- | Complete description of /hs2bf/ behavior
data Command
  = ShowMessage String
  | Interpret Option String
  | Compile Option String

data Language
  = LangCore String
  | LangGM String
  | LangSAM String
  | LangBF
  deriving (Show, Eq, Ord)

-- | All /global options/
data Option = Option
  { addrSpace :: Int,
    verbose :: Bool,
    debug :: Bool,
    tolang :: Language
  }


execCommand :: Command -> IO ()
execCommand (ShowMessage x) = putStrLn x
execCommand (Interpret opt from) =
  partialChain opt from $
    ( error "Core interpreter is not implemented",
      error "Core interpreter is not implemented",
      f GMachine.interpret,
      f GMachine.interpretR,
      f SAM.interpret,
      f SAM.interpret,
      f Brainfuck.interpret
    )
  where
    f g = runProcessWithIO (\x -> setio >> g x)
    setio = hSetBuffering stdin NoBuffering >> hSetBuffering stdout NoBuffering
execCommand (Compile opt from) =
  partialChain opt from $
    ( f Core.pprint,
      f Core.pprint,
      f GMachine.pprint,
      f GMachine.pprint,
      f SAM.pprint,
      f SAM.pprint,
      f Brainfuck.pprint
    )
  where
    f g = runProcessWithIO (putStr . g)

partialChain opt from (c0, c1, g0, g1, s0, s1, b) = do
  dir <- Paths_hs2bf.getDataDir
  let (mod, env) = analyzeName from dir
  xs <- Front.collectModules env mod
  let cr = xs >>= Front.compile
      cr' = cr >>= Core.simplify
      gm = cr' >>= Core.compile
      gm' = gm >>= GMachine.simplify
      sam = gm' >>= GMachine.compile
      sam' = sam >>= SAM.simplify
      bf = sam' >>= SAM.compile
  case tolang opt of
    LangCore "" -> c0 cr
    LangCore "s" -> c1 cr'
    LangGM "" -> g0 gm
    LangGM "r" -> g1 gm'
    LangSAM "" -> s0 sam
    LangSAM "f" -> s1 sam'
    LangBF -> b bf

analyzeName :: String -> FilePath -> (String, Front.ModuleEnv)
analyzeName n lib = (takeBaseName n, Front.ModuleEnv [dirPrefix ++ takeDirectory n, lib])
  where
    dirPrefix = if isAbsolute n then "" else "./"
