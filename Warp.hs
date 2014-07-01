module Main(main) where

import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import Application (app)
import Control.Monad (unless)

main = do
	args <- getArgs
	unless (length args > 0) $ fail "Must Provide at Least One Directory"
	run 8080 $ app args
