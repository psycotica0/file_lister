{-# LANGUAGE OverloadedStrings #-}
module Mime (get_videos) where

--import Network.Mime (defaultMimeLookup)
import Lister (ListEntry(..), filter_files)
import Data.ByteString.Char8 (isPrefixOf, ByteString)
import Data.List (isSuffixOf)

get_videos :: ListEntry -> Maybe ListEntry
get_videos = filter_files is_video
	where
	is_video = isPrefixOf "video/" . defaultMimeLookup

defaultMimeLookup :: FilePath -> ByteString
defaultMimeLookup path | (".mkv" `isSuffixOf` path) = "video/mkv"
defaultMimeLookup _ = "text/plain"
