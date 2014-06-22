module Main(main) where

import Lister (build_list)
import System.Environment (getArgs)
import Control.Monad ((<=<))

main = mapM_ (print <=< build_list) =<< getArgs 
