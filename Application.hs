{-# LANGUAGE OverloadedStrings #-}
module Application where

import Lister(build_list, collapse_dirs, ListEntry(..), display_name)
import Mime(get_videos)
import Network.Wai (Application, responseBuilder)
import Network.HTTP.Types.Status(ok200)
import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.ByteString(fromLazyByteString)
import Data.Monoid ((<>), mconcat)
import Data.ByteString.Lazy.Char8 (pack)
import Control.Monad (join, (<=<))

html_template :: Builder -> Builder
html_template body = fromLazyByteString "<!DOCTYPE html><html><head><title>File Listing</title></head><body>" <> body <> fromLazyByteString "</body></html>"

html_output :: ListEntry -> Builder
html_output list = html_template $ fromLazyByteString "<h1>File Listing:</h1>" <> fromLazyByteString "<ol>" <> html_output_inner list <> fromLazyByteString "</ol>"
	where
	html_output_inner f@(File _) = mconcat $ fmap fromLazyByteString ["<li>", pack $ display_name f, "</li>"]
	html_output_inner d@(Directory p children) = (mconcat $ fmap fromLazyByteString ["<li><h1>", pack $ display_name d, "</h1><ol>"]) <> mconcat (fmap html_output_inner children) <> fromLazyByteString "</ol></li>"

empty_output :: Builder
empty_output = html_template $ fromLazyByteString "<h1>No Files Found</h1>"

filters :: Maybe ListEntry -> Maybe ListEntry
filters = join . fmap (collapse_dirs <=< get_videos)

app :: Application
app req cont = do
	list <- fmap filters $ build_list "test"
	cont $ responseBuilder ok200 [] $ maybe empty_output html_output list
