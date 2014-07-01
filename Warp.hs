module Main(main) where

import Network.Wai.Handler.Warp (run)
import Application (app)

main = run 8080 app
