{-# LANGUAGE CPP #-}
module Main
  ( main -- :: IO ()
  ) where
import           Control.Monad
import           Data.List
import           System.Directory
import           System.FilePath

import           Language.Haskell.Liquid.Liquid (liquid)
import           System.Environment             (getArgs)

main :: IO ()
main = allSources >>= liquid'
  where
    -- | Run liquid haskell with custom options for this
    -- package.
    liquid' :: [FilePath] -> IO ()
    liquid' files = do
      -- arguments the user passes
      userArgs <- getArgs
      -- headers that need to be included
      headers <- includes

      -- We have to link in the C code
      let linkArgs = ["--cfiles=cbits/half.c"]

      -- we also need all the right package databases
      pkgs <- packageDbArgs

      -- and we need some extra arguments, too
      let extraArgs = ["--short-names"]

      let allArgs = headers
                 ++ linkArgs
                 ++ pkgs
                 ++ extraArgs
                 ++ userArgs

      if null userArgs
        -- if the user has no args, run over all files
        then liquid (allArgs ++ files)
        else do
          -- otherwise, run with the given input
          liquid allArgs

    packageDbArgs :: IO [String]
    packageDbArgs = do
      -- NOTE: cdir must be absolute, because it is passed directly to
      -- GHC, but liquid haskell does not work relative to the cwd. that
      -- means a relative path will result in a 'file not found' error.
      cdir <- getCurrentDirectory >>= makeAbsolute

      -- TODO FIXME: non new-build style support.
      let mkDB p = [ "--ghc-option=-package-db", "--ghc-option="++p ]
      confFile1 <- head <$> getFiles ".conf" (cdir </> "dist-newstyle" </> "packagedb")
      confFile2 <- head <$> getFiles ".conf" (cdir </> "dist-newstyle" </> "build")
      -- we can get the ghc version here, as a total hack
      let ghcVer = snd (splitFileName (takeDirectory confFile1))

      -- first db: in $HOME/.cabal/store
      hdir <- getHomeDirectory >>= makeAbsolute
      pkgdb1 <- return (hdir </> ".cabal" </> "store" </> ghcVer </> "package.db")
      -- second db: the directory confFile1 is in
      pkgdb2 <- return (takeDirectory confFile1)
      -- third db: the directory confFile2 is in
      pkgdb3 <- return (takeDirectory confFile2)
      return $ [ "--ghc-option=-no-user-package-db"
               ] ++ mkDB pkgdb1
                 ++ mkDB pkgdb2
                 ++ mkDB pkgdb3

    -- | Generate all the proper arguments needed to include files
    includes :: IO [String]
    includes = do
      -- NOTE: cdir must be absolute, because it is passed directly to
      -- GHC, but liquid haskell does not work relative to the cwd. that
      -- means a relative path will result in a 'file not found' error.
      cdir <- getCurrentDirectory >>= makeAbsolute

      -- TODO FIXME: non new-build style support.
      files <- getFiles ".h" (cdir </> "dist-newstyle" </> "build")

      -- fix up a magic incantation to get the right headers for liquid
      -- haskell
      let args  = concatMap (\x -> ["-optP-include", "-optP" ++ x]) files
          args' = map ("--ghc-option="++) args

      -- NOTE: here, cbits does _not_ need to be absolute, because this arg
      -- is passed to liquid haskell and not ghc, so it will properly resolve
      -- it to an absolute path for us.
      return ("-icbits":args')

-- | Get all the library source files.
allSources :: IO [FilePath]
allSources = getFiles ".hs" "Data"

-- | Find all files of a given extension in a directory, and return
-- their paths.
getFiles :: String -> FilePath -> IO [FilePath]
getFiles ext root = filter (isSuffixOf ext) <$> go root
  where
    go dir = do
      (dirs, files) <- getFilesAndDirectories dir
      (files ++) . concat <$> mapM go dirs

getFilesAndDirectories :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories dir = do
  c <- fmap (dir </>) . filter (`notElem` ["..", "."]) <$> getDirectoryContents dir
  (,) <$> filterM doesDirectoryExist c <*> filterM doesFileExist c
