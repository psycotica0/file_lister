module Lister (
	ListEntry (..),
	build_list,
	filter_files,
	prune_dirs,
	collapse_dirs,
	display_name,
	merge,
) where

import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import Data.Bool.HT (if')
import Control.Applicative (pure)
import Data.Monoid (mconcat)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf, span, sort, group)
import Data.Function (on)

data ListEntry = Directory FilePath [ListEntry] | File FilePath deriving (Show)

-- Currently, it's just basename
display_name :: ListEntry -> String
display_name (Directory p _) = basename p
display_name (File p) = basename p

instance Eq ListEntry where
	(==) = (==) `on` display_name

instance Ord ListEntry where
	compare = compare `on` display_name

basename :: FilePath -> FilePath
basename = handle . break (== '/')
	where
	handle (x, []) = x
	handle (x, "/") = x
	handle (_, '/':xs) = basename xs

build_list :: FilePath -> IO (Maybe ListEntry)
build_list path = handleDir =<< doesDirectoryExist path
	where
	handleDir True = buildDirectory . mapM (build_list . prependPath) =<< getActualDirs
	handleDir False = fmap handleFile $ doesFileExist path
	handleFile True = Just $ File path
	handleFile False = Nothing
	filterDirs p = not $ "." `isPrefixOf` p
	prependPath p = path ++ "/" ++ p
	buildDirectory = fmap $ fmap (Directory path) . sequence
	getActualDirs = fmap (filter filterDirs) $ getDirectoryContents path

removeNothings :: [Maybe a] -> [a]
removeNothings = foldr func []
  where
  func Nothing acc = acc
  func (Just a) acc = a:acc

-- This function applies a 
filter_files :: (FilePath -> Bool) -> ListEntry -> Maybe ListEntry
filter_files = post_process . func_filter_files 
func_filter_files pred f@(File p) = if (pred p) then Just f else Nothing
func_filter_files _ d = Just d

prune_dirs :: ListEntry -> Maybe ListEntry
prune_dirs = post_process func_prune_dirs
func_prune_dirs f@(File _) = Just f
func_prune_dirs (Directory _ []) = Nothing
func_prune_dirs d = Just d 

collapse_dirs :: ListEntry -> Maybe ListEntry
collapse_dirs = post_process func_collapse_dirs
func_collapse_dirs f@(File _) = Just f
func_collapse_dirs (Directory _ [x]) = Just x
func_collapse_dirs (Directory _ []) = Nothing
func_collapse_dirs d = Just d

post_process :: (ListEntry -> Maybe ListEntry) -> ListEntry -> Maybe ListEntry
post_process func e@(File _) = func e
post_process func (Directory path children) = func $ Directory path $ removeNothings $ fmap (post_process func) children

merge :: ListEntry -> ListEntry -> ListEntry
merge first@(File _) _ = first
merge first@(Directory _ _) (File _) = first
merge (Directory p c1) (Directory _ c2) = Directory p $ fmap (foldr1 $ flip merge) $ group $ sort $ c1 ++ c2
