module Main(main) where

import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import Application (preloaded_app, preload_list)
import System.IO (hFlush, stdout)
import Control.Monad (unless)

main = do
	args <- getArgs
	unless (length args > 0) $ fail "Must Provide at Least One Directory"
	putStr "Reading File List..."
	hFlush stdout
	list <- preload_list args
	putStrLn " Done"
	putStrLn "Listening on 8080"
	run 8080 $ preloaded_app list
