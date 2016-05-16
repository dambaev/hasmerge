import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import System.Cmd
import System.Directory
import Data.List
import Data.Version 

main = defaultMainWithHooks (defaultUserHooks { postBuild = buildDll })
   where
   buildDll _ _ pkg info = do putStrLn "Building Dll..."
                              setCurrentDirectory (buildDir info)
                              let buildCmd = cmd pkg info
                              putStrLn buildCmd
                              system buildCmd
                              let dll = dllFile pkg
                              let cpDllCmd = "cp " ++ dll ++ " " ++ (name pkg) ++ "\\" ++ dll
                              putStrLn cpDllCmd
                              system cpDllCmd
                              return ()
   ghcExe :: LocalBuildInfo -> String
   ghcExe info = "\"" ++ "ghc" ++ "\""
   mainOFile :: PackageDescription -> String
   mainOFile pd = "HS" ++ (name pd) ++ "-" ++ (showVersion (pkgVersion (package pd))) ++ ".o"
   cmd :: PackageDescription -> LocalBuildInfo -> String
   cmd pd i = (ghcExe i) ++ " --mk-dll -o " ++ (dllFile pd) ++ " " ++ (mainOFile pd) ++ " " ++ (packages i)
   packages :: LocalBuildInfo -> String
   packages i = foldl1 (\x y -> x ++ " " ++ y) (map showPackage (externalPackageDeps i))
   showPackage :: PackageIdentifier -> String
   showPackage pi = "-package " ++ packageId pi
   name :: PackageDescription -> String
   name = pkgName . package 
   dllFile :: PackageDescription -> String
   dllFile pd = (name pd) ++ ".dll"

