{-# LANGUAGE OverloadedStrings #-}
module Mime (get_videos) where

import Network.Mime (defaultMimeType, mimeByExt, defaultMimeMap)
import Lister (ListEntry(..), filter_files)
import Data.ByteString.Char8 (isPrefixOf, ByteString)
import Data.List (isSuffixOf)
import Data.Text (pack)
import qualified Data.Map as Map (fromAscList, union)

get_videos :: ListEntry -> Maybe ListEntry
get_videos = filter_files is_video
	where
	is_video = isPrefixOf "video/" . mimeByExt mimeMap defaultMimeType . pack

mimeMap = defaultMimeMap `Map.union` Map.fromAscList [
	("3gp", "video/3gpp")
	, ("avi", "video/x-msvideo")
	, ("divx", "video/x-msvideo")
	, ("flv", "video/x-flv")
	, ("m4v", "video/x-m4v")
	, ("mkv", "video/x-matroska")
	, ("mov", "video/quicktime")
	, ("mp4", "video/mp4")
	, ("mpg", "video/mpeg")
	, ("wmv", "video/x-ms-wmv")
	]
