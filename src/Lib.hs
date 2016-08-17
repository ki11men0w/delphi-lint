{- -*- coding:utf-8 -*- -}

module Lib
    (
      findFiles
    , filterPascalFiles
    , filterPasFiles
    , filterDfmFiles
    , fileName
    , isFile
    , isDir
    ) where

import System.Directory.Tree (
                              AnchoredDirTree( (:/) ),
                              DirTree(..),
                              filterDir,
                              readDirectoryWith,
                             )
import System.FilePath (takeExtension)
import Data.Char (toLower)


fileName :: DirTree a -> FilePath
fileName (File name _) = name
fileName (Dir name _) = name
fileName _ = mempty

isFile :: DirTree a -> Bool
isFile (File _ _) = True
isFile _ = False

isDir :: DirTree a -> Bool
isDir (Dir _ _) = True
isDir _ = False


dirTreeWithFilter :: FilePath -> IO (DirTree FilePath)
dirTreeWithFilter rootDir = do
  _:/tree <- readDirectoryWith return rootDir
  return $ filterDir dropGarbage tree
    where dropGarbage x = isFile x || (isDir x && fileName x /= ".git")


findFiles :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findFiles cond rootDir = do
  tree <- dirTreeWithFilter rootDir
  return $ filter cond $ foldr (:) [] tree

filterPascalFiles :: FilePath -> Bool
filterPascalFiles p = any (check p) [".pas", ".dpr"]

filterPasFiles :: FilePath -> Bool
filterPasFiles p = any (check p) [".pas"]

filterDfmFiles :: FilePath -> Bool
filterDfmFiles p = any (check p) [".dfm"]

check x = ((map toLower . takeExtension) x ==)
