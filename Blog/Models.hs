 {-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
   MultiParamTypeClasses, TemplateHaskell, TypeFamilies, RecordWildCards, FlexibleInstances,
   UndecidableInstances, OverlappingInstances #-}

module Blog.Models where

import Control.Applicative  ( (<$>), optional )
import Control.Monad ( liftM )
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Control.Monad.Trans  ( liftIO )
import Data.Acid            ( AcidState, Update, Query, makeAcidic, openLocalState )
import Data.Acid.Advanced   ( update', query' )
import Data.Data            ( Data, Typeable )
import Data.IxSet           ( Indexable(..), IxSet(..), (@=), Proxy(..), getOne
                            , ixFun, ixSet )
import qualified Data.IxSet as IxSet
import Data.SafeCopy        ( SafeCopy, base, deriveSafeCopy )
import Data.Text            ( Text )
import Data.Text.Lazy       ( toStrict )
import qualified Data.Text  as Text
import Data.Time            ( UTCTime(..), getCurrentTime )
import           Text.Blaze ( (!) )
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Control.Applicative.Indexed ( IndexedFunctor(..), IndexedApplicative(..) )
import Happstack.Server.HSP.HTML ()
import Happstack.Auth
import Happstack.State hiding ( Update, Query, Proxy )


-- Every post must have unique id to be found in DB
newtype PostId = PostId { unPostId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy)

--instance Version PostId
-- $(deriveSerialize ''PostId)

data Status =
    Draft
  | Published
    deriving (Eq, Ord, Data, Typeable)

-- Using Haskell Template
-- deriveSafeCopy creates an instance of the SafeCopy class for BlogState
-- SafeCopy is class for versioned serialization, deserilization, and migration.
--instance Version Status
-- $(deriveSerialize ''Status)
$(deriveSafeCopy 0 'base ''Status)

-- Data Structure describing our Blog Post
data Post = Post
    { postId  :: PostId
    , atitle  :: Text
    , author  :: Text
    , abody   :: Text
    , date    :: UTCTime
    , status  :: Status
--    , tags    :: [Text]
    }
    deriving (Eq, Ord, Data, Typeable)

--instance Version Post
$(deriveSafeCopy 0 'base ''Post)
-- $(deriveSerialize ''Post)

-- To use IxSet we need to make every key to have unique type
newtype Title     = Title Text    deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Author    = Author Text   deriving (Eq, Ord, Data, Typeable, SafeCopy)
-- newtype Tags      = Tags Text      deriving (Eq, Ord, Data, Show, Typeable, SafeCopy)


-- Here we make our Post to be the instance of IxSet - class that defines 
-- the keys for a Post so that we can store it in an IxSet.
-- and we pass to ixFun functions that extract values form keys.
instance Indexable Post where
    empty = ixSet [ ixFun $ \bp -> [ postId bp ], 
    				ixFun $ \bp -> [ Title  $ atitle bp  ], 
    				ixFun $ \bp -> [ Author $ author bp ], 
    				ixFun $ \bp -> [ status bp ], 
    				-- ixFun $ \bp -> map Tags (tags bp), 
    				ixFun $ (:[]) . date
                  ]

-- Data Structure of Blog
data Blog = Blog
    { nextPostId :: PostId
    , posts      :: IxSet Post
--    , allTags    :: [Text] 
    }
    deriving (Data, Typeable)

--instance Version Blog
-- $(deriveSerialize ''Blog)
$(deriveSafeCopy 0 'base ''Blog)

-- We should make our state Blog an instance of Component
-- and set dependency of AuthState
--instance Component Blog where
--  type Dependencies Blog = AuthState :+: End
--  initialValue = initialBlogState 

-- We define initial state of our DB as empty ixSet 
initialBlogState :: Blog
initialBlogState =
    Blog { nextPostId = PostId 1, 
           posts      = empty
--         ,  allTags    = []
         }
        
-- Create a new, post and add it to the database
newPost :: Post -> UTCTime -> Update Blog Post
newPost inputPost pubDate =
    do b@Blog{..} <- get 
       let pst = Post {  postId = nextPostId
                       , atitle = atitle inputPost
                       , author = author inputPost
                       , abody  = abody inputPost
                       , date   = date inputPost
                       , status = status inputPost
--                      , tags   = tags inputPost
                      } 
       put $ b { nextPostId = succ nextPostId
               , posts      = IxSet.insert pst posts
--               , allTags    = tags pst ++ allTags
               }
       return pst

-- Udate the post in the database (indexed by PostId)
-- this signature means that updatePost is a function in the State monad
-- that updates our Blog and returns () - nth
updatePost :: Post -> Update Blog ()
updatePost updatedPost =
    do b@Blog{..} <- get
       put $ b { posts = IxSet.updateIx (postId updatedPost) updatedPost posts
               }

-- Get post by key. The same situation, just query Blog for post by id
-- and returns maybe post               
postById :: PostId -> Query Blog (Maybe Post)
postById pid =
     do Blog{..} <- ask
        return $ getOne $ posts @= pid

postsByStatus :: Status -> Query Blog [Post]
postsByStatus status =
    do Blog{..} <- ask
       return $ IxSet.toDescList (Proxy :: Proxy UTCTime) $ posts @= status

deletePost :: Maybe Post -> Update Blog ()
deletePost postToDel = do
  case postToDel of
    Just p -> do b@Blog{..} <- get
                 put $ b { posts      = IxSet.deleteIx (postId p) posts
                        -- , allTags    = filter (\x -> elem x (tags p)) allTags
                         }

--getTags :: Query Blog [Text]
--getTags = 
--  do Blog{..} <- ask
--     return  allTags

-- Here we make our Blog data structure and Update and Query functions
-- acid-state events
$(makeAcidic ''Blog
  [ 'newPost, 
    'updatePost,
    'postById,
    'postsByStatus,
    'deletePost
   --, 'getTags
  ])
