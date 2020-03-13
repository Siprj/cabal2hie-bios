{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( parseCabalFile
    , getExistingFiles
    , componentToHieFile
    , BiosCabalComponent(..)
    ) where

import Distribution.PackageDescription.Parsec
import Distribution.Verbosity
import Distribution.Types.GenericPackageDescription
    ( GenericPackageDescription(..)
    , FlagAssignment
    , mkFlagAssignment
    )
import Distribution.ModuleName
import Distribution.PackageDescription.Configuration
import Distribution.Types.Benchmark
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.TestSuiteInterface
import Distribution.Types.BuildInfo
import Distribution.Types.Executable
import Distribution.Types.Library
import Distribution.Types.LibraryName
import Distribution.Types.PackageDescription
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.TestSuite
import Distribution.Types.UnqualComponentName
import Distribution.Types.Version
import Distribution.Compiler
    ( CompilerInfo
    , CompilerId(CompilerId)
    , CompilerFlavor(GHC)
    , unknownCompilerInfo
    )
import Distribution.System (buildPlatform)
import Distribution.Simple (AbiTag(NoAbiTag))
import Control.Lens.Getter
import Data.Text
import Data.Semigroup
import Data.Traversable
import Control.Monad
import System.FilePath
import System.Directory


data CabalComponentType = Lib | Exe | Test | Bench
  deriving (Show, Eq)

data BiosCabalComponent = BiosCabalComponent
    { name :: Text
    , componentType :: CabalComponentType
    , filepaths :: [FilePath]
    }
  deriving (Show, Eq)

componentToHieFile :: BiosCabalComponent -> Text
componentToHieFile BiosCabalComponent{..} = Data.Text.unlines $ case componentType of
    Lib -> mconcat $ fmap (pathAndComponent ("      component: \"lib:" <> name <> "\"")) filepaths
    Exe -> mconcat $ fmap (pathAndComponent ("      component: \"exe:" <> name <> "\"")) filepaths
    Test -> mconcat $ fmap (pathAndComponent ("      component: \"test:" <> name <> "\"")) filepaths
    Bench -> mconcat $ fmap (pathAndComponent ("      component: \"bench:" <> name <> "\"")) filepaths
  where
    pathAndComponent compStr filepath =
        [ "    - path: \"" <> pack filepath <> "\""
        , compStr
        ]

parseCabalFile :: FilePath -> IO [BiosCabalComponent]
parseCabalFile cabalFilePath = do
    (packageDescription, _) <- readGenericPackageDescription verbose cabalFilePath
        >>= either
            (const $ fail "Internal error (finalizePD is missing dependences!!!")
            pure
            . finalizePD'
    toComponents packageDescription
  where
    flags :: FlagAssignment
    flags = mkFlagAssignment []

    componentReques :: ComponentRequestedSpec
    componentReques = ComponentRequestedSpec True True

    compilerInfo :: CompilerInfo
    compilerInfo = unknownCompilerInfo (CompilerId GHC $ mkVersion [8,6,5]) NoAbiTag

    finalizePD' = finalizePD flags componentReques (const True) buildPlatform compilerInfo []

    toComponents :: PackageDescription -> IO [BiosCabalComponent]
    toComponents PackageDescription{..} = mconcat
        [ maybe (pure []) (fmap (: []) . toComponent Lib getLibraryMetaData) library
        , mapM (toComponent Lib getLibraryMetaData) subLibraries
        , mapM (toComponent Exe getExecutableMetaData) executables
        , mapM (toComponent Test getTestSuiteMetaData) testSuites
        , mapM (toComponent Test getBenchmarkMetaData) benchmarks
        ]
      where
        toComponent
            :: CabalComponentType
            -> (a -> (Text, [FilePath], [FilePath]))
            -> a
            -> IO BiosCabalComponent
        toComponent cabalComponentType f a = do
            let (name, files, dirs) = f a
            existingFiles <- getExistingFiles (dropFileName cabalFilePath) files dirs
            pure $ BiosCabalComponent name cabalComponentType existingFiles

        getLibraryMetaData :: Library -> (Text, [FilePath], [FilePath])
        getLibraryMetaData library@Library{..} =
            ( libraryNameToStr (packageIdToStr package) libName
            , flip addExtension "hs" . toFilePath <$> explicitLibModules library
            , hsSourceDirs libBuildInfo)

        getExecutableMetaData :: Executable -> (Text, [FilePath], [FilePath])
        getExecutableMetaData executable@Executable{..} =
            ( pack $ unUnqualComponentName exeName
            , modulePath : (flip addExtension "hs" . toFilePath <$> exeModules executable)
            , hsSourceDirs buildInfo)

        getTestSuiteMetaData :: TestSuite -> (Text, [FilePath], [FilePath])
        getTestSuiteMetaData testSuite@TestSuite{..} =
            ( pack $ unUnqualComponentName testName
            , mainFile <> (flip addExtension "hs" . toFilePath <$> testModules testSuite)
            , hsSourceDirs testBuildInfo)
          where
            mainFile = case testInterface of
                (TestSuiteExeV10 _ f) -> [f]
                _ -> []

        getBenchmarkMetaData :: Benchmark -> (Text, [FilePath], [FilePath])
        getBenchmarkMetaData benchmark@Benchmark{..} =
            ( pack $ unUnqualComponentName benchmarkName
            , flip addExtension "hs" . toFilePath <$> benchmarkModules benchmark
            , hsSourceDirs benchmarkBuildInfo)

getExistingFiles :: FilePath -> [FilePath] -> [FilePath] -> IO [FilePath]
getExistingFiles root files dirs = fmap (makeRelative root)
    <$> filterM doesFileExist [root </> dir </> file | file <- files, dir <- dirs]

packageIdToStr :: PackageId -> Text
packageIdToStr id = pack . unPackageName $ pkgName id

libraryNameToStr :: Text -> LibraryName -> Text
libraryNameToStr def LMainLibName = def
libraryNameToStr def (LSubLibName name) = pack $ unUnqualComponentName name
