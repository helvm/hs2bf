module ScriptGoldenSpec (test_golden) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import HS2Lazy
import HS2Lazy.Compiler.Lambda
import HS2Lazy.Syntax.Lambda
import System.FilePath (takeBaseName, (<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Text.Pretty.Simple

inputFiles :: [FilePath]
inputFiles = unsafePerformIO (findByExtension [".hs"] ("examples"))

test_golden :: TestTree
test_golden =
  testGroup "Golden tests" $
    map createTests inputFiles
  where
    createTests inFile = unsafePerformIO $ do
      source <- BSL.readFile inFile
      let result = compile $ (BSL.toString source)
      let ((expandedSki, compiledSki), (optimizedExpr, expandedExpr, compiledExpr)) = result
      let lambda = compileToLambda optimizedExpr
      let baseName = takeBaseName inFile
      pure $
        testGroup
          baseName
          [ goldenVsString
              "SKI output"
              (".golden" </> "scripts" </> "lazy" </> "expanded" </> baseName <.> "lazy")
              (pure $ BSL.fromString $ renderSKI expandedSki)
          ]
