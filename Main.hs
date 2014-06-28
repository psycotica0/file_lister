module Main(main) where

import Lister (build_list, filter_files)
import System.Environment (getArgs)
import Control.Monad ((<=<))

main = mapM_ (print . fmap (filter_files (elem 'a')) <=< build_list) =<< getArgs 
