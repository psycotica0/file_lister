module Lister (
	ListEntry (..),
	build_list,
	filter_files,
) where

import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import Data.Bool.HT (if')
import Control.Applicative (pure)
import Data.Monoid (mconcat)
import Data.Maybe (fromMaybe)

data ListEntry = Directory FilePath [ListEntry] | File FilePath deriving (Show)

build_list :: FilePath -> IO (Maybe ListEntry)
build_list path = handleDir =<< doesDirectoryExist path
	where
	handleDir True = buildDirectory . mapM (build_list . prependPath) =<< getActualDirs
	handleDir False = fmap handleFile $ doesFileExist path
	handleFile True = Just $ File path
	handleFile False = Nothing
	filterDirs "." = False
	filterDirs ".." = False
	filterDirs _ = True
	prependPath p = path ++ "/" ++ p
	buildDirectory = fmap $ fmap (Directory path) . sequence
	getActualDirs = fmap (filter filterDirs) $ getDirectoryContents path

filter_files :: (FilePath -> Bool) -> ListEntry -> Maybe ListEntry
filter_files pred file@(File path) = if' (pred path) (Just file) Nothing
filter_files pred (Directory path files) = fmap (Directory path) $ mconcat $ fmap (fmap pure . filter_files pred) files
