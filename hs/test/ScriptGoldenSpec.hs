module ScriptGoldenSpec (test_golden) where

import qualified Data.ByteString.Lazy as BSL
import qualified Paths_hs2bf
import HS2BF
import System.FilePath (takeBaseName, (<.>), (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import HS2BF.Brainfuck as Brainfuck
import System.IO.Unsafe (unsafePerformIO)

inputFolder :: FilePath
inputFolder = "examples"

inputFiles :: IO [FilePath]
inputFiles = findByExtension [".hs"] inputFolder

test_golden :: TestTree
test_golden = unsafePerformIO $ do
    files <- inputFiles
    tests <- mapM createTest files
    pure $ testGroup "Golden tests" tests

  where
    createTest :: FilePath -> IO TestTree
    createTest inFile = do
        let baseName = takeBaseName inFile
        let opt = Option { addrSpace = 2, verbose = False, debug = False, tolang = LangBF }
        output <- partialChain opt inFile $
            (error "Core not needed", error "Core not needed",
             error "GMachine not needed", error "GMachine not needed",
             error "SAM not needed", error "SAM not needed",
             \bf -> do
                 pure $ BSL.fromStrict $ Brainfuck.pprint bf
            )
        let goldenFile = ".golden" </> "hs2bf" </> baseName <.> "bf"
        pure $ goldenVsString ("Brainfuck output: " ++ baseName) goldenFile output
