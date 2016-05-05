import           Data.Maybe
import           Distribution.PackageDescription    hiding (Flag)
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils
import           System.Directory

-- http://codinginfinity.me/post/2015-04-18/haskell_and_cpp

main = defaultMainWithHooks simpleUserHooks
  {
    preConf = makeExtLib
  , confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs
  , postCopy = copyExtLib
  , postClean = cleanExtLib
  }

makeExtLib :: Args -> ConfigFlags -> IO HookedBuildInfo
makeExtLib _ flags = do
  let verbosity = fromFlag $ configVerbosity flags
  rawSystemExit verbosity "env"
    ["make", "--directory=."]
  return emptyHookedBuildInfo

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
  let packageDescription = localPkgDescr localBuildInfo
      lib = fromJust $ library packageDescription
      libBuild = libBuildInfo lib
  dir <- getCurrentDirectory
  return localBuildInfo {
    localPkgDescr = packageDescription {
      library = Just $ lib {
        libBuildInfo = libBuild {
          extraLibDirs = (dir ++ "/dist/build/staticlib") : extraLibDirs libBuild
        }
      }
    }
  }

copyExtLib :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyExtLib _ flags pkg_descr lbi = do
  let libPref =
        libdir . absoluteInstallDirs pkg_descr lbi . fromFlag . copyDest $ flags
  let verbosity = fromFlag $ copyVerbosity flags
  -- copies from the output of makefile to libPref
  -- make sure the path below matches the makefile output file
  rawSystemExit verbosity "cp" ["dist/build/staticlib/libctensor.a", libPref]
  rawSystemExit verbosity "cp" ["dist/build/dynamiclib/libctensor.so", libPref]

cleanExtLib :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
cleanExtLib _ flags _ _ =
  let verbosity = fromFlag $ cleanVerbosity flags
  in rawSystemExit verbosity "env" ["make", "--directory=.", "clean"]