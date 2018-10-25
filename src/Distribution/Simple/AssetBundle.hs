{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.Simple.AssetBundle
  (
    -- * Motivation
    --
    -- $Motivation

    -- * Goal
    --
    -- $Goal

    -- * Quick Start
    --
    -- $QuickStart

    -- * The API
    --
    postCopy,
    depsWithData

    -- * Launch Scripts
    --
    -- ** Why?
    --
    -- $WhyLaunchScripts

    -- ** Structure
    --
    -- $StructureLaunchScript

    -- ** Adding Custom Code
    --
    -- $NaughtyBits
  )
where
import Control.Monad(filterM, mapM)
import Distribution.InstalledPackageInfo as InstalledPackageInfo(InstalledPackageInfo, dataDir, sourcePackageId)
import Distribution.Package(packageId)
import Distribution.PackageDescription(PackageDescription, exeName, Executable)
import Distribution.Simple(Args, PackageId, pkgName)
import Distribution.Simple.LocalBuildInfo(LocalBuildInfo, withPrograms, absoluteInstallDirs, bindir, substPathTemplate, componentUnitId, installedPkgs, withExeLBI, progPrefix, progSuffix)
import Distribution.Simple.PackageIndex(topologicalOrder)
import Distribution.Simple.Program(Program, simpleProgram, runProgram, defaultProgramDb)
import Distribution.Simple.Program.Db(requireProgram)
import Distribution.Simple.Setup(CopyFlags, fromFlag, copyDest)
import Distribution.Simple.Utils (withTempDirectory, copyDirectoryRecursiveVerbose, createDirectoryIfMissingVerbose, installExecutableFile, copyFileVerbose, normaliseLineEndings, rawSystemExit)
import Distribution.Text(display)
import Distribution.Verbosity(normal, Verbosity)
import System.Directory (getTemporaryDirectory, doesDirectoryExist, withCurrentDirectory, setOwnerExecutable, setPermissions, executable, getPermissions)
import System.FilePath ((</>), (<.>), takeFileName , takeDirectory)
import Data.List(intercalate)
import System.IO(withFile, hPutStr, IOMode(WriteMode))
import Distribution.System(buildOS, OS(Windows))

-- $Motivation
-- Currently building an executable with <https://www.haskell.org/cabal/users-guide/ Cabal> or <https://docs.haskellstack.org/en/stable/README/ Stack> will create a static binary in their respective
-- `bin` directory but if it or any of its dependencies reference a local resource such as an
-- image, config file, fonts etc. it is very tedious and error-prone to deploy. This package eases that pain a bit.
--
-- This package aims to solve the problem of bundling a Haskell executable with its
-- <https://www.haskell.org/cabal/users-guide/developing-packages.html#pkg-field-data-files assets> *and* those
-- in its dependencies by providing a couple of functions you can call from your project's /Setup.hs/ so that doing
-- `{cabal, stack} build` also creates a <https://en.wikipedia.org/wiki/Zip_(file_format) zip> of the executable
-- and all of the assets it needs along with a launch script (a batch file on Windows and a shell script otherwise) that runs the executable
-- adjusting various environment variables so that it can find them at runtime. Even if you're not deploying an executable directly
-- to your users this package still makes it considerably simpler to dry-run on a test machine or in a Docker container.
--
-- This package emphatically does not replace a reproducible build system such as <https://nixos.org/nix/ Nix>
-- because it does not solve the problem of packaging up runtime library dependencies or really anything outside
-- your Cabal file's <https://www.haskell.org/cabal/users-guide/developing-packages.html#pkg-field-data-files data-files stanza>.
--
-- Why zip format instead of an OSX Bundle, a Windows <https://portableapps.com/ Portable App> or a Linux <https://appimage.org/ AppImage>?
-- Firstly, creating a zip is /much/ simpler to and works well across platforms, secondly, as outlined above, this package does not create a
-- standalone installation and finally 'zip' is ubiqitous and probably comes with your system so you don't have to install anything extra to use
-- this library.

-- $Goal
-- Just to be clear on what to expect, if your project provides the executables /my-awesome-app/ and /my-awesomer-app/ using this library
-- will result in two /zip/ files, /my-awesome-app_bundled.zip/ and /my-awesomer-app_bundled.zip/ somewhere deep within your
-- /dist/ or /.stack-work/ directory depending on whether you're using Cabal or Stack. These archives will contain all your assets
-- with the executable and a launch script called /run\.bat/ on Windows and /run\.sh/ otherwise. See the 'Launch Scripts' section below
-- for more details on what it contains.

-- $QuickStart
-- First this library needs to be added as a build-time dependency in your Cabal file with a top level <https://www.haskell.org/cabal/users-guide/developing-packages.html#custom-setup-scripts custom-setup stanza>:
--
-- @
-- custom-setup
--   setup-depends:
--       Cabal >=1.24.1 && <3
--     , asset-bundle
--     , base >=4.4
-- @
--
-- And at minimum a /Setup.hs/ that uses this library to bundle all the executables in the package:
--
-- @
-- import Distribution.PackageDescription(PackageDescription)
-- import Distribution.Simple(defaultMainWithHooks, simpleUserHooks, Args, postCopy)
-- import qualified Distribution.Simple.AssetBundle as Bundle (postCopy, depsWithData)
-- import Distribution.Simple.LocalBuildInfo(LocalBuildInfo)
-- import Distribution.Simple.Setup(CopyFlags)
--
-- main = defaultMainWithHooks (simpleUserHooks { postCopy = myPostCopy })
--
-- myPostCopy :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
-- myPostCopy args copyFlags pd localBuildInfo = do
--   (postCopy simpleUserHooks) args copyFlags pd localBuildInfo
--   deps <- Bundle.depsWithData localBuildInfo
--   Bundle.postCopy Nothing deps args copyFlags pd localBuildInfo
-- @
--
-- I'll try to explain it since it's quite a bit messier than the standard:
--
-- @
-- import Distribution.Simple
-- main = defaultMain
-- @
--
-- The Cabal API exposes a record of functions which it calls <https://www.stackage.org/haddock/lts-11.4/Cabal-2.0.1.1/Distribution-Simple.html#t:UserHooks hooks> that are called
-- at the appropriate time in the build cycle which you can override with custom implementations. If, for example, you wanted custom behavior after your lib/executable has
-- finished building you'd override the <https://www.stackage.org/haddock/lts-11.4/Cabal-2.0.1.1/Distribution-Simple.html#v:postBuild postBuild> hook. If it helps XMonad configuration
-- works a lot like this.
--
-- The /main/ function above overrides the <https://www.stackage.org/haddock/lts-11.4/Cabal-2.0.1.1/Distribution-Simple.html#v:postCopy postCopy> hook to use functions provided
-- by this library (they are explained in further down):
--
-- @
-- main = defaultMainWithHooks (simpleUserHooks { postCopy = myPostCopy })
-- @
--
-- The custom hook first lets the default implementation work in:
--
-- @
--   (postCopy simpleUserHooks) args copyFlags pd localBuildInfo
-- @
--
-- And then gathers up all of your project's dependencies using this library's 'depsWithData' function and passes them to the provided 'postCopy':
--
-- @
--   deps <- Bundle.depsWithData localBuildInfo
--   Bundle.postCopy Nothing deps args copyFlags pd localBuildInfo
-- @

pathsModuleEnv :: PackageId -> String
pathsModuleEnv packageId =
  showPkgName (pkgName packageId)
  where
    showPkgName = map fixchar . display
    fixchar '-' = '_'
    fixchar c   = c

env :: PackageId -> String
env packageId = (pathsModuleEnv packageId) ++ "_" ++ "datadir"

bat :: String -> [(String, String)] -> String -> String
bat executableName envDataDirMap extra =
  let contents =
        [
          "@echo off"
        , "setlocal"
        , "set dir=%~dp0"
        , "set PATH=%PATH%;%dir%"
        , "set _Tmp=%1"
        , ":loop"
        , "if not '%2==' set _Tmp=%_Tmp% %2"
        , "shift"
        , "if not '%2==' goto loop"
        ] ++
        map (\(env,dataDir) -> "set " ++ env ++ "=%dir%\\" ++ dataDir) envDataDirMap ++
        [
          normaliseLineEndings extra
        , (executableName <.> "exe") ++ " %_Tmp%"
        , "set _Tmp="
        ]
  in
    intercalate "\n" contents

sh :: String -> [(String, String)] -> String -> String
sh executableName envDataDirMap extra =
  let contents =
        [
          "#!/bin/sh"
        , "HERE=\"$(dirname \"$(readlink -f \"${0}\")\")\""
        , "export PATH=\"${HERE}\":\"${PATH}\""
        , "EXEC=" ++ executableName
        ] ++
        map (\(env, dataDir) -> "export " ++ env ++ "=\"${HERE}\"/" ++ dataDir) envDataDirMap ++
        [
          normaliseLineEndings extra
        , "exec \"${EXEC}\" \"$@\""
        ]
  in
    intercalate "\n" contents

zipProgram :: Program
zipProgram = (simpleProgram "zip")

createZip :: Verbosity -> PackageDescription -> Maybe LocalBuildInfo -> FilePath -> IO FilePath
createZip verbosity pd mLbi zipContentsPath =
  let zipFile = zipContentsPath <.> "zip"
  in do
  (zipProg , _) <- requireProgram verbosity zipProgram (maybe defaultProgramDb withPrograms mLbi)
  runProgram verbosity zipProg ["-r", "-X", zipFile, zipContentsPath]
  return zipFile

{-|
  Generate a list of your project's dependencies by passing in 'LocalBuildInfo' which is available
  to most of the Cabal API <https://www.stackage.org/haddock/lts-11.4/Cabal-2.0.1.1/Distribution-Simple-UserHooks.html#t:UserHooks hooks>.
-}
depsWithData :: LocalBuildInfo -> IO [InstalledPackageInfo]
depsWithData localBuildInfo =
  filterM (doesDirectoryExist . InstalledPackageInfo.dataDir) (topologicalOrder (installedPkgs localBuildInfo))

{-|
  A custom <https://www.stackage.org/haddock/lts-11.4/Cabal-2.0.1.1/Distribution-Simple.html#v:postCopy postCopy> hook
  gathers executables in the project and zips them up all the assets in this project and it's dependencies.

  The zip is named /\<executable-name\>_bundled.zip/ and it's copied
  to <https://www.stackage.org/haddock/lts-11.4/Cabal-2.0.1.1/Distribution-Simple-InstallDirs.html#v:bindir the Cabal-local bin directory>.

  It takes a list of 'InstalledPackageInfo' , which can be generated using
  'depsWithData', containing this project's dependencies so it can gather up the
  resources, a list of extra files (no directories!) ['FilePath'] you may want
  to include that are just copied to the root of zip archive, and @Maybe
  (Executable -> String)@ which allows you to add arbitrary content to the
  launch script before an executable is run and; more on that below in the
  'Adding Custom Code', in the 'Launch Scripts'section below. Most of the time
  you'll probably want to pass in 'Nothing'.
-}
postCopy:: Maybe (Executable -> String) -> [FilePath] -> [InstalledPackageInfo] -> Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postCopy preRun extraFiles deps args copyFlags pd localBuildInfo  =
  let installDirs = absoluteInstallDirs pd localBuildInfo (fromFlag (copyDest copyFlags))
      binPref = bindir installDirs
      prefixOrSuffix componentLocalBuildInfo pathTemplate =
        substPathTemplate (packageId pd) localBuildInfo (componentUnitId componentLocalBuildInfo) pathTemplate
  in do
  temp <- getTemporaryDirectory
  withTempDirectory
    normal
    temp
    "bundleBuild."
    (\tmpDir -> withExeLBI pd
        localBuildInfo
        (\executable componentLocalBuildInfo ->
            withCurrentDirectory
              tmpDir
              (
                let installedExeName =
                      prefixOrSuffix componentLocalBuildInfo (progPrefix localBuildInfo) ++
                      display (exeName executable) ++
                      prefixOrSuffix componentLocalBuildInfo (progSuffix localBuildInfo)
                    zipDir = installedExeName ++ "_bundled"
                    installRunner extension makeRunner =
                      let zipFile = zipDir </> "run" <.> extension
                          runner = makeRunner installedExeName
                                     (map (\dep -> (env (InstalledPackageInfo.sourcePackageId dep) , takeFileName (InstalledPackageInfo.dataDir dep))) deps)
                                     (
                                       case preRun of
                                        Nothing -> ""
                                        Just f -> f executable
                                     )
                      in do
                      withFile zipFile WriteMode (\handle -> hPutStr handle runner)
                      p <- getPermissions zipFile
                      setPermissions zipFile (p { executable = True})
                in do
                createDirectoryIfMissingVerbose normal False zipDir
                mapM_ (\dep -> copyDirectoryRecursiveVerbose normal (takeDirectory (InstalledPackageInfo.dataDir dep)) zipDir) deps
                case buildOS of
                  -- Windows has issues with copy file permissions
                  Windows -> mapM_ (\f -> rawSystemExit normal "cp" [f, zipDir]) extraFiles
                  _ -> mapM_ (\f -> copyFileVerbose normal f zipDir) extraFiles
                case buildOS of
                  Windows -> installRunner "bat" bat
                  _ -> installRunner "sh" sh
                let exe = case buildOS of { Windows -> installedExeName <.> ".exe" ; _ -> installedExeName }
                installExecutableFile normal (binPref </> exe) (zipDir </> exe)
                zipFile <- createZip normal pd (Just localBuildInfo) zipDir
                copyFileVerbose normal zipFile (binPref </> (takeFileName zipFile))
              )
        )
    )

-- $WhyLaunchScripts
-- Launch scripts are needed because when 'Cabal' builds your project it generates a module behind the scenes which hard-codes a number of environment variables that point
-- to needed libraries and other data so that your app can find them at runtime. The module, typically called /Paths_\<package-name\>/ contains something like:
--
-- @
-- module Paths_AwesomeApp (
--   ...
--   ) where
-- ...
-- getDataDir = catchIO (getEnv \"AwesomeApp_datadir\") (\_ -> return datadir)
-- ...
-- @
--
-- The important bit here is the @...(getEnv \"AwesomeApp_datadir\")...@  which tells your project that it should look for assets in the location specified by the \"AwesomeApp_datadir\"
-- environment variable at runtime and if it doesn't exist fallback on some hardcoded path. All of your project's library dependencies contain a module like this and use a similar
-- looking environment variable (/\<library-name\>_datadir/) for their runtime lookup. The purpose of the launch scripts is to point these environemnt variables to the assets local to the zip file
-- before launching the app.

-- $StructureLaunchScript
-- On Windows the launch script /run\.bat/ looks something like:
--
-- @
-- \@echo off
-- setlocal
-- set dir=%~dp0
-- set PATH=%PATH%;%dir%
-- set _Tmp=%1
-- :loop
-- if not '%2==' set _Tmp=%_Tmp% %2
-- shift
-- if not '%2==' goto loop
-- set AwesomeApp_datadir=%dir%\\AwesomeApp-0.1.0.0\\
-- set AwesomeDependency_datadir=%dir%\\AwesomeDependency-0.1.0.0\\
-- REM from you, dear reader
-- awesomeApp %_Tmp%
-- set _Tmp=
-- @
--
-- The @%~dp0@ sets @dir@ to the directory in which the script is located and then the @PATH@ is updated.
--
-- The rest of the gobbledygook from @set _Tmp ..@ all the way to the second @if not '%2=='@ is necessary because the Windows DOS prompt only sees the first <https://en.wikibooks.org/wiki/Windows_Batch_Scripting#Command-line_arguments 9 command line arguments>.
-- The workaround is a loop which accumulates each command line argument into @_Tmp@ and then splices that into the executable invocation.
--
-- The next couple of lines set the Cabal hard-coded environment variables to point to data directories in the archive containing the assets.
--
-- The @REM from you, dear reader@ bit is explained in the next section.
--
-- On OSX, Linux and the BSD the launch script /run\.sh/ is quite a bit simpler:
--
-- @
-- #!\/bin\/sh
-- HERE="$(dirname "$(readlink -f "${0}")")"
-- export PATH="${HERE}":"${PATH}"
-- EXEC=awesomeApp
-- export AwesomeApp_datadir="${HERE}"/AwesomeApp-0.1.0.0
-- export AwesomeDependency_datadir="${HERE}"/AwesomeDependency-0.1.0.0
-- # from you, dear reader
-- exec "${EXEC}" "$@"
-- @
--
-- No surprises here, except the @# from you, dear reader@ line which is explained next. We set @HERE@ to the directory containing the launcher, update @PATH@, set the environment variables as we did above and call the executable.

-- $NaughtyBits
-- Before the executable is invoked in the launcher scripts you have the opportunity to insert any arbitrary code you desire. It is passed in via the
-- first argument to 'postCopy' and is spliced in at the line marked @from you, dear reader@ in the previous section.
