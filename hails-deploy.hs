module Main where

import Prelude hiding (catch)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.PrettyPrint
import Distribution.Package
import Distribution.Compiler
import Distribution.ModuleName ( fromString, ModuleName )
import Distribution.Version
import Distribution.Verbosity
import Distribution.Text

import Language.Haskell.Extension

import System.Directory
import System.FilePath
import System.Posix.Temp
import System.IO
import System.Process
import System.Exit

import Data.Maybe
import Data.List ((\\))

import Control.Exception
import Control.Monad

-- | Find .cabal file in given directory
findCabalFile :: FilePath -> IO FilePath
findCabalFile dir = do
  files <- getDirectoryContents dir
  let cabalFiles = filter ((==".cabal") . takeExtension) files
  case length cabalFiles of
        1 -> return $ head cabalFiles
        _ -> throwIO . userError $ "Expecting 1 cabal file"

-- | Rewrite the given cabal file, creating a .bak backup
rewriteCabalFile :: FilePath -> IO ()
rewriteCabalFile file = do
  gpkg0 <- readPackageDescription normal file
  let pkg0 = packageDescription gpkg0
  case lookup "x-hails-server" $ customFieldsPD pkg0 of
    Nothing -> throwIO . userError $ "Expecting top-level x-hails-user field"
    Just server -> do
      let cabalVersion = orLaterVersion $ Version [1,10] []
          pkg1 = emptyPackageDescription { package = package pkg0
                                         , license = license pkg0
                                         , licenseFile = licenseFile pkg0 
                                         , buildType = Just Simple
                                         , specVersionRaw = Right cabalVersion 
                                         , synopsis = "Hails-deploy script" }
          gpkg1 = GenericPackageDescription { packageDescription = pkg1
                                            , genPackageFlags = []
                                            , condLibrary = Nothing
                                            , condExecutables = []
                                            , condTestSuites = []
                                            , condBenchmarks = [] }
      case condLibrary gpkg0 of
        (Just (CondNode _ deps _)) -> do
          let mModuleName = simpleParse server :: Maybe ModuleName
              moduleName  = fromJust mModuleName
          when (isNothing mModuleName) $ throwIO $ userError "Invalid module"
          (fp, h) <- mkstemp (dropFileName file </> "hails_dummyXXXXXX")
          writeMainFile h moduleName
          hClose h
          renameFile fp (fp ++ ".hs")
          let exeN = dropExtension $ takeFileName fp
              exe = Executable { exeName    = exeN
                               , modulePath = exeN ++ ".hs"
                               , buildInfo  = emptyBuildInfo {
                                     defaultLanguage = Just Haskell2010
                                   , defaultExtensions  = [EnableExtension Safe]
                                   , options = [(GHC, ["-fpackage-trust"])] } }
              depsWCore = addCoreHailsPackages deps
              gpkg2 = gpkg1 { condExecutables =
                                [(exeName exe, CondNode exe depsWCore [])] }
          copyFile file (file ++ ".bak")
          writeGenericPackageDescription file gpkg2
        _ -> throwIO . userError $ "Expecting top-level Library"

-- | Write a dummy make file. The goal behind the file is to get the
-- Safe Haskell flags to fail at build vs. when loading app with
-- hails binary. Observe thtat the file does not actually do anything
writeMainFile :: Handle -> ModuleName -> IO ()
writeMainFile h serverModule = do
  hPutStrLn h   "module Main where"
  hPutStrLn h   "import Hails.App (AppReqHandler)"
  hPutStrLn h $ "import safe " ++ display serverModule ++ " ( server )\n"
  hPutStrLn h   "main :: IO ()"
  hPutStrLn h   "main = return ()\n"
  hPutStrLn h   "dummyServer :: AppReqHandler"
  hPutStrLn h   "dummyServer = server"

-- | Hails depends on several packages made available to all the apps
-- in the \'pre-built\' cabal-dev directory. This function removes all
-- of them fro mthe app's dependency list (since older versions may
-- have security vulns) and appaned an unversioned list.
addCoreHailsPackages :: [Dependency] -> [Dependency]
addCoreHailsPackages ds = 
  let xfm = \d@(Dependency (PackageName n) _) ->
              if n `elem` coreHailsPackages
                then Dependency (PackageName n) anyVersion else d
  in map xfm ds


-- | Core hails packages
coreHailsPackages :: [String]
coreHailsPackages = [ "dclabel", "lio"
                    , "iterIO" , "iterio-server"
                    , "hails"  , "hails-cjail"]

-- | Packages trusted by hails
otherTrustedPackages :: [String]
otherTrustedPackages = [ "aeson"
                       , "base64-bytestring"
                       , "blaze-html"
                       , "pandoc" ]

--
--
--

-- | Execute cabal-dev. If the process terminates abnormally, throw
-- an exception with its @stderr@.
cabal_dev :: Maybe FilePath -> String -> [String] -> IO ()
cabal_dev msbox cmd args = do
  let bin  =  "cabal-dev"
      sbox = maybe [] (\x -> ["--sandbox="++x]) msbox
  (ex, _, e) <- readProcessWithExitCode bin (cmd : (sbox ++ args)) ""
  when (ex /= ExitSuccess) $ throwIO . userError $
                              "cabal-dev add-source falied:\n" ++ e

-- | Copy an directory recursively
copyBaseCabalDevDir :: FilePath -> FilePath -> IO ()
copyBaseCabalDevDir base new = do
  (ex, _, _) <- readProcessWithExitCode "cp" ["-r", base, new] ""
  when (ex /= ExitSuccess) $ throwIO . userError $ "Copy base cabal-dev failed"

main = do
  let appDir          = "/tmp/app"
      base_cabal_dir  = "/tmp/base_cabal_dev"
  inDir appDir $ do
    -- Rewrite cabal file
    findCabalFile "." >>= rewriteCabalFile 
    -- Make sure there is no cabal-dev set up:
    exists <- doesDirectoryExist "cabal-dev"
    when exists $ throwIO . userError $ "cabal-dev sandbox must not exist"
    -- Copy base cabal-dir containing core packages installed and tursted
    copyBaseCabalDevDir base_cabal_dir "cabal-dev"
    -- Install all the package dependencies:
    cabal_dev Nothing "install-deps" []
    -- Configure package:
    cabal_dev Nothing "configure" []
    -- Build package:
    cabal_dev Nothing "build" []


-- | Execute an action in a directory
inDir :: FilePath -> IO a -> IO a
inDir dir act = do
  dir0 <- getCurrentDirectory
  setCurrentDirectory dir
  act `finally` setCurrentDirectory dir0

ignoreErr :: IO () -> IO ()
ignoreErr act = act `catch` (\e@(SomeException _) -> return ())
