{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell , FlexibleInstances,
             FlexibleContexts, UndecidableInstances, OverlappingInstances,
             MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Main where
-- import my modules
import Blog.Controllers
import Blog.Models
import Blog.Views

import Control.Exception  ( bracket )
import Control.Monad      ( msum )
import Data.Acid          ( AcidState, Query, Update, makeAcidic, openLocalState )
import Data.Acid.Local    ( createCheckpointAndClose )
import Happstack.Server   ( ServerPart, Response, Browsing(..), serveDirectory,
                            toResponse, nullConf, nullDir, simpleHTTP, ok, dirs,
                            dir, BodyPolicy(..), decodeBody, defaultBodyPolicy, methodM, Method(GET,POST))
import Happstack.Auth

main :: IO ()
main =
    do bracket (openLocalState initialBlogState)
               (createCheckpointAndClose)
               (\acid ->
                    simpleHTTP nullConf (handlers acid))
      
-- Here all routes described and mapped to handlers
handlers :: AcidState Blog -> ServerPart Response
handlers acid =
    do decodeBody (defaultBodyPolicy "/tmp/" 0 1000000 1000000)
       msum [ nullDir >> homePage acid,
              dir "newpost" $ newPage acid,
			        dir "editpost" $ editPage acid,
			        dir "post" $ viewPage acid,
		        	dir "drafts" $ draftsPage acid,
              dir "delete" $ deletePage acid,
			--serving static files
       		    dir "style" $ serveDirectory EnableBrowsing ["basic.css"] "/home/johnpaul/Study/FP/project/BlogHappstack/static",
              dir "back" $ serveDirectory EnableBrowsing ["background.jpg"] "/home/johnpaul/Study/FP/project/BlogHappstack/static",
              notFoundHandler acid
            ]