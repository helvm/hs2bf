module ScriptGoldenSpec (test_golden) where

import qualified Data.ByteString.Lazy as BSL
import qualified Paths_hs2bf
import HS2BF
import System.FilePath (takeBaseName, (<.>), (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import HS2BF.Brainfuck as Brainfuck
import HS2BF.SAM as SAM
import HS2BF.GMachine as GMachine
import HS2BF.Core as Core
import HS2BF.Front as Front
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
        dir <- Paths_hs2bf.getDataDir

        let (modName, env) = analyzeName inFile dir

        xs  <- Front.collectModules env modName
        let cr  = xs >>= Front.compile

        let cr' = cr >>= Core.simplify

        let gm  = cr' >>= Core.compile
        let gm' = gm  >>= GMachine.simplify

        let sam  = gm' >>= GMachine.compile
        let sam' = sam >>= SAM.simplify

        let bf  = sam' >>= SAM.compile

        let run x =
                case runIdentity (runExceptT x) of
                    Left errs -> error (show errs)
                    Right v   -> v

        let mkGolden name ext val =
                goldenVsString
                    name
                    (".golden" </> name </> baseName <.> ext)
                    (pure $ BSL.fromString val)

        pure $ testGroup baseName
            [ mkGolden "core"        "core"        (Core.pprint        (run cr))
            , mkGolden "core-simpl"  "core"  (Core.pprint        (run cr'))
            , mkGolden "gm"          "gm"          (GMachine.pprint    (run gm))
            , mkGolden "gm-simpl"    "gm"    (GMachine.pprint    (run gm'))
            , mkGolden "sam"         "sam"         (SAM.pprint         (run sam))
            , mkGolden "sam-simpl"   "sam"   (SAM.pprint         (run sam'))
            , mkGolden "bf"          "bf"          (Brainfuck.pprint   (run bf))
            ]