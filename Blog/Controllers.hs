{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards
  , TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Blog.Controllers where

import Blog.Models
import Blog.Views
import Control.Applicative  ( (<$>), optional )
import Control.Exception    ( bracket )
import Control.Monad        ( msum, mzero, liftM )
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Control.Monad.IO.Class
import Data.Acid                         ( AcidState )
import Data.Acid.Advanced                ( query', update' )
import Data.Typeable
import Data.Text            ( Text )
import Data.Text.Lazy       ( toStrict )
import qualified Data.Text  as Text
import Data.Time            ( UTCTime(..), getCurrentTime )
import Happstack.Server     ( ServerPart, Method(POST, HEAD, GET), Response, decodeBody
                            , defaultBodyPolicy, dir, lookRead, lookText', lookText, method
                            , notFound, nullConf, nullDir, ok, seeOther, simpleHTTP
                            , toResponse )
import Happstack.Server.HSP.HTML ()
import HSP.ServerPartT
import Happstack.State
import Happstack.State.ComponentSystem
import Text.Blaze
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

-- This is home page where all articles are displayed
homePage :: AcidState Blog -> ServerPart Response
homePage acid =
    do published <- query' acid (PostsByStatus Published)
       case published of
        [] -> ok $ appTemplate acid "Home page" [] $ "You have no published posts at this time."
        _ -> ok $ appTemplate acid "Home page" [] (mapM_ postHtml published)


newPage :: AcidState Blog -> ServerPart Response
newPage acid = 
  do msum  [ do method GET
                ok $ appTemplate acid "New post page" [] newPostForm
           , do method POST
                ttl   <- lookText' "atitle"
                athr  <- lookText' "author"
                -- tgs   <- lookText' "tags"
                bdy   <- lookText' "body"
                now   <- liftIO $ getCurrentTime
                stts  <- do s <- lookText' "status"
                            case s of
                              "save"    -> return Draft
                              "publish" -> return Published
                              _         -> mzero
                let nPost = Post { 
                                  postId = PostId 0,
                                  atitle  = ttl,
                                  author = athr,
                                  abody   = bdy,
                                  date   = now,
                                  status = stts
                                  --, tags   = map Text.strip $ Text.splitOn "," tgs
                                }
                update' acid (NewPost nPost now)
                case (status nPost) of
                    Published ->
                      seeOther ("/"::Text)
                               (toResponse ())
                    Draft     ->
                      seeOther ("/drafts"::Text)
                               (toResponse ()) ]


-- Handler of /editpost creates form and handles data from POST 
editPage :: AcidState Blog -> ServerPart Response
editPage acid =
    do pid   <- PostId <$> lookRead "id"
       mMsg  <- optional $ lookText "msg"
       mPost <- query' acid (PostById pid)
       case mPost of
         Nothing ->
             notFound $ appTemplate acid "Edit page - no such post" [] $ do "Could not find a post with this id" 
                                                                            H.toHtml (unPostId pid)
         (Just p@(Post{..})) ->
             msum [ do method GET
                       ok $ appTemplate acid "Edit page" [] (editPostForm p), 
                    do method POST
                       ttl   <- lookText' "atitle"
                       athr  <- lookText' "author"
                       -- tgs   <- lookText' "tags"
                       bdy   <- lookText' "body"
                       now   <- liftIO $ getCurrentTime
                       stts  <- do s <- lookText' "status"
                                   case s of
                                     "save"    -> return Draft
                                     "publish" -> return Published
                                     _         -> mzero
                       let updatedPost =
                               p { atitle  = ttl
                                 , author = athr
                                 , abody   = bdy
                                 , date   = now
                                 , status = stts
                               --  , tags   = map Text.strip $ Text.splitOn "," tgs
                                 }
                       update' acid (UpdatePost updatedPost)
                       case status of
                         Published ->
                           seeOther ("/"::Text)
                                    (toResponse ())
                         Draft     ->
                           seeOther ("/editpost?msg=saved&id=" ++ (show $ unPostId pid))
                                    (toResponse ())
                  ]

                 where lookText' = fmap toStrict . lookText

-- View of one blog page
viewPage :: AcidState Blog -> ServerPart Response
viewPage acid =
    do pid <- PostId <$> lookRead "id"
       mPost <- query' acid (PostById pid)
       case mPost of
         Nothing ->
             notFound $ appTemplate acid "Post view page - no such post" [] $ do "Could not find a post with id "
                                                                                 H.toHtml (unPostId pid)
         (Just p) ->
             ok $ appTemplate acid (atitle p) [] $ do
                 (postHtml p)

-- Show a list of all unpublished blog posts, that have been saved
draftsPage :: AcidState Blog -> ServerPart Response
draftsPage acid =
    do drafts <- query' acid (PostsByStatus Draft)
       case drafts of
         [] -> ok $ appTemplate acid "Drafts page" [] $ "You have no unpublished posts at this time." 
         _ ->
             ok $ appTemplate acid "Home page" [] (H.ol $ mapM_ editDraftLink drafts)
    where
      editDraftLink Post{..} =
        H.a ! A.href (H.toValue $ "/editpost?id=" ++ show (unPostId postId)) $ H.toHtml atitle


deletePage :: AcidState Blog -> ServerPart Response
deletePage acid = 
    do pid   <- PostId <$> lookRead "id"
       mPost <- query' acid (PostById pid)
       do posts <- update' acid (DeletePost mPost)
          seeOther ("/"::Text) (toResponse ())

notFoundHandler :: AcidState Blog -> ServerPart Response
notFoundHandler acid = 
  notFound $ appTemplate acid "No such page" [] $ do "Could not find this page"

--serveTags :: MonadIO m => AcidState Blog -> m [Text]
--serveTags acid = query' acid GetTags 

--arrangeTags :: MonadIO m => m [Text] -> String
--arrangeTags acid = 
--  do t <- (serveTags acid)  
--     foldl (\x y -> x ++ (Text.unpack y) ++ ">" ++ (Text.unpack y) ++ "</a>  ") "<a href='"  t