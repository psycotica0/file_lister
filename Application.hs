{-# LANGUAGE OverloadedStrings #-}
module Application where

import Lister(build_list, collapse_dirs)
import Mime(get_videos)
import Network.Wai (Application, responseLBS)
import Network.HTTP.Types.Status(ok200)

app :: Application
app req cont = cont $ responseLBS ok200 [] "Hello Fucker!"
