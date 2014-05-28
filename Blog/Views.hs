{-# LANGUAGE TemplateHaskell , OverloadedStrings, RecordWildCards #-}

module Blog.Views where
 
import Blog.Models 

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State  ( modify,put,get,gets,MonadState )
import Data.Acid                         ( AcidState )
import Data.Acid.Advanced                ( query', update' )
import Data.Text            ( Text )
import Data.Text.Lazy       ( toStrict )
import qualified Data.Text  as Text
import Happstack.Server
import Happstack.State
import Text.Blaze                         as H
import Text.Blaze.Html4.Strict            as H hiding ( map )
import Text.Blaze.Html4.Strict.Attributes as A hiding ( dir, label, title )
import Text.Blaze.Html5 (footer)

-- Main template applied to every page
appTemplate :: AcidState Blog -> Text -> [Html] -> Html -> Response
appTemplate acid inputTitle inputHeaders bodyContent =
  toResponse $
        H.html $ do
            H.head $ do
                title $ toHtml inputTitle
                link ! rel "stylesheet" ! type_ "text/css" ! href "/style"
                link ! href "http://fonts.googleapis.com/css?family=PT+Sans&subset=latin,cyrillic,latin-ext" ! rel "stylesheet" ! type_ "text/css"
                H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
                sequence_ inputHeaders
            H.body $ do 
                H.div ! class_ "wrapper" $ do
                  H.div ! class_ "upper" $ do
                    H.div ! class_ "bar" $ do
                      H.a ! A.href "/" $ "Home"
                      "   "
                      H.a ! A.href "/drafts" $ "Drafts"
                      "   "
                      H.form ! A.enctype "multipart/form-data"
                             ! A.action "/newpost?msg='create'" $ H.button $ "New Post"
                    H.div ! class_ "logo" $ 
                      H.h1 $ toHtml ("Happstack Blog"::Text)
                  H.div ! class_ "content" $ bodyContent
                H.br
                -- H.div ! class_ "tags" $ toHtml ("blabla"::String)
                H.br
                footer $ toHtml ("Eugenia Oshurko, 2012. Functional Programming"::Text)


-- Blog article Html representation
postHtml  :: Post -> Html
postHtml (Post{..}) =
  H.div ! A.class_ "post" $ do
    H.h1 $ H.toHtml atitle
    H.div ! A.class_ "author" $ do "author: "    >> H.toHtml author
    H.div ! A.class_ "date"   $ do "published: " >> H.toHtml (show date)
    -- H.div ! A.class_ "tags"   $ do "tags: "       >> H.toHtml (Text.intercalate ", " tags)
    H.div ! A.class_ "bdy" $ H.toHtml abody
    H.div ! A.class_ "post-footer" $ do
     H.span $ H.a ! A.href (H.toValue $ "/post?id=" ++ 
                            show (unPostId postId)) $ "Full View"
     H.span $ " "
     H.span $ H.a ! A.href (H.toValue $ "/editpost?id=" ++
                            show (unPostId postId)) $ "Edit"
     H.span $ " "
     H.span $ H.a ! A.href (H.toValue $ "/delete?id=" ++
                            show (unPostId postId)) $ "Delete"

newPostForm :: Html
newPostForm =
  H.form ! A.enctype "multipart/form-data" ! A.action "/newpost" ! A.method "POST" $ do
    H.label "title " ! A.for "title"
    H.input ! A.type_ "text"
            ! A.name "atitle"
            ! A.id "atitle"
            ! A.size "80"
    H.br
    H.label "author " ! A.for "author"
    H.input ! A.type_ "text"
            ! A.name "author"
            ! A.id "author"
            ! A.size "40"
    H.br
    --H.label "tags " ! A.for "tags"
    --H.input ! A.type_ "text"
    --        ! A.name "tags"
    --        ! A.id "tags"
    --        ! A.size "40"
    --H.br
    H.label "body " ! A.for "body"
    H.br
    H.textarea ! A.cols "80" ! A.rows "20" ! A.name "body" $ H.toHtml $ (""::Text) 
    H.br
    H.button ! A.name "status" ! A.value "publish" $ "Publish"
    -- H.form ! A.enctype "multipart/form-data"
             --                ! A.method "POST"
               --              ! A.action "newpost" $ H.button $ "Publish"
    H.button ! A.name "status" ! A.value "save"    $ "Save as draft"


editPostForm :: Post -> Html
editPostForm inputPost = 
  H.form ! A.enctype "multipart/form-data" ! A.method "POST" ! A.action (H.toValue $ "/editpost?id=" ++ (show $ unPostId (postId inputPost))) $ do
    H.label "title" ! A.for "title"
    H.input ! A.type_ "text"
            ! A.name "atitle"
            ! A.id "atitle"
            ! A.size "80"
            ! A.value (H.toValue $ atitle inputPost)
    H.br
    H.label "author" ! A.for "author"
    H.input ! A.type_ "text"
            ! A.name "author"
            ! A.id "author"
            ! A.size "40"
            ! A.value (H.toValue $ author inputPost)
    H.br
    --H.label "tags" ! A.for "tags"
    --H.input ! A.type_ "text"
    --        ! A.name "tags"
    --        ! A.id "tags"
    --        ! A.size "40"
    --        ! A.value (H.toValue $ Text.intercalate ", " (tags inputPost))
    --H.br
    H.label "body" ! A.for "body"
    H.br
    H.textarea ! A.cols "80" ! A.rows "20" ! A.name "body" $ H.toHtml $ abody inputPost
    H.br
    H.button ! A.name "status" ! A.value "publish" $ "Publish"
    H.button ! A.name "status" ! A.value "save"    $ "Save as draft"