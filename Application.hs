{-# LANGUAGE OverloadedStrings #-}
module Application where

import Lister(build_list, collapse_dirs, ListEntry(..), display_name, merge)
import Mime(get_videos)
import Network.Wai (Application, responseBuilder)
import Network.HTTP.Types.Status(ok200)
import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.ByteString(fromLazyByteString)
import Data.Monoid ((<>), mconcat)
import Data.ByteString.Lazy.Char8 (pack)
import Control.Monad (join, (<=<))
import Data.List (sort)

html_template :: Builder -> Builder
html_template body = fromLazyByteString "<!DOCTYPE html><html><head><title>File Listing</title></head><body>" <> body <> fromLazyByteString "</body></html>"

html_output :: ListEntry -> Builder
html_output list = html_template $ fromLazyByteString "<h1>File Listing:</h1>" <> fromLazyByteString "<ol>" <> html_output_inner list <> fromLazyByteString "</ol>"
	where
	html_output_inner f@(File p) = mconcat $ fmap fromLazyByteString ["<li><a href=\"", pack $ p, "\">" , pack $ display_name f, "</a></li>"]
	html_output_inner d@(Directory p children) = (mconcat $ fmap fromLazyByteString ["<li><h1>", pack $ display_name d, "</h1><ol>"]) <> mconcat (fmap html_output_inner children) <> fromLazyByteString "</ol></li>"

empty_output :: Builder
empty_output = html_template $ fromLazyByteString "<h1>No Files Found</h1>"

filters :: Maybe ListEntry -> Maybe ListEntry
filters = join . fmap (collapse_dirs <=< get_videos)

sort_entries :: ListEntry -> ListEntry
sort_entries f@(File _) = f
sort_entries (Directory p children) = Directory p $ sort children

maybe_merge :: Maybe ListEntry -> Maybe ListEntry -> Maybe ListEntry
maybe_merge Nothing a = a
maybe_merge a Nothing = a
maybe_merge (Just a) (Just b) = Just $ merge a b

app :: [FilePath] -> Application
app roots req cont = do
	merged <- fmap (foldr1 maybe_merge) $ mapM build_list roots
	cont $ responseBuilder ok200 [] $ maybe empty_output html_output $ fmap sort_entries $ filters merged
