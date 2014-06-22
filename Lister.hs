module Lister where

import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)

data ListEntry = Directory FilePath [ListEntry] | File FilePath deriving (Show)

build_list :: FilePath -> IO (Maybe ListEntry)
build_list path = handleDir =<< doesDirectoryExist path
	where
	handleDir True = fmap (fmap (Directory path) . sequence) . mapM (build_list . prependPath) =<< fmap (filter filterDirs) (getDirectoryContents path)
	handleDir False = fmap handleFile $ doesFileExist path
	handleFile True = Just $ File path
	handleFile False = Nothing
	filterDirs "." = False
	filterDirs ".." = False
	filterDirs _ = True
	prependPath p = path ++ "/" ++ p

