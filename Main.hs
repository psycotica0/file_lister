module Main(main) where

import Lister (build_list, filter_files)
import Mime (get_videos)
import System.Environment (getArgs)
import Control.Monad ((<=<), join)

main = mapM_ (print . join . fmap get_videos <=< build_list) =<< getArgs 
