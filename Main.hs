module Main(main) where

import Lister (build_list, collapse_dirs)
import Mime (get_videos)
import System.Environment (getArgs)
import Control.Monad ((<=<), join)

main = mapM_ (print . join . fmap (collapse_dirs <=< get_videos) <=< build_list) =<< getArgs 
