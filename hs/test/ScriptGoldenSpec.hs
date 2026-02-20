module ScriptGoldenSpec (test_golden) where

import qualified Data.ByteString.Lazy as BSL
import qualified Paths_hs2bf
import HS2BF
import System.FilePath (takeBaseName, (<.>), (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import HS2BF.Brainfuck as Brainfuck
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Control.Monad.Trans.Except (runExceptT)
import Data.Functor.Identity (runIdentity)

inputFolder :: FilePath
inputFolder = "examples"

inputFiles :: IO [FilePath]
inputFiles = do
    files <- findByExtension [".hs"] inputFolder
    pure $ filter (\f -> takeBaseName f /= "Prelude") files

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
        let goldenFile = ".golden" </> "bf" </> baseName <.> "bf"

        exceptT <- partialChain opt inFile $
            ( error "Core not needed", error "Core not needed",
              error "GMachine not needed", error "GMachine not needed",
              error "SAM not needed", error "SAM not needed",
              pure
            )

        let eResult = runIdentity $ runExceptT exceptT

        output <- case eResult of
            Left errs -> error $ "Partial chain failed: " ++ show errs
            Right bf  -> pure $ BSL.fromString $ Brainfuck.pprint bf

        pure $ goldenVsString ("Brainfuck output: " ++ baseName) goldenFile (pure output)
