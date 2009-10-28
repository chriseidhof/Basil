{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE UndecidableInstances #-}

module CoreData2 where

import Data.Record.Label (mkLabels, label)
import Generics.MultiRec.Base
import Generics.MultiRec.TH
import Basil
import qualified Data.Map as M
import Prelude hiding (log)
import Basil.Data.TList4
import Control.Monad.Trans (lift)

data User = UserC {name :: String, password :: String, age :: Int} deriving (Show)
data Post = PostC {title :: String, body :: String} deriving (Show)
data Comment = CommentC {text :: String} deriving Show
data Page    = PageC {content :: String} deriving Show

data Blog :: * -> * where
  User    :: Blog User
  Post    :: Blog Post
  Comment :: Blog Comment
  Page    :: Blog Page

$(mkLabels [''User, ''Post, ''Comment, ''Page])
$(deriveConstructors [''User, ''Post, ''Comment, ''Page])
$(deriveSystem ''Blog [''User, ''Post, ''Comment, ''Page] "PFBlog")
type instance PF Blog = PFBlog

instance ERModel Blog ERRelationsBlog where
  relations = TCons4 User    Post authorPosts 
            $ TCons4 User Comment authorComments 
            $ TCons4 Post Comment postComments 
            $ TCons4 User    User parentChildren
            $ TCons4 User    Page homepages
            $ TNil4

type ERRelationsBlog = ((One `T` Many) User Post
                       ,((One `T` Many) User Comment
                       ,((One `T` Many) Post Comment 
                       ,((One `T` Many) User User
                       ,((One `T` One)  User Page
                       ,()
                       )))))
authorPosts    = Rel "author" One User `mkRelation` Rel "posts"    Many Post
authorComments = Rel "author" One User `mkRelation` Rel "comments" Many Comment
postComments   = Rel "post"   One Post `mkRelation` Rel "comments" Many Comment
parentChildren = Rel "parent" One User `mkRelation` Rel "children" Many User
homepages      = Rel "author" One User `mkRelation` Rel "author" One Page

type T m1 m2 t1 t2 = To Blog m1 m2 t1 t2


exampleUser  n = UserC    n "test" 24
examplePost    = PostC    "fipo" "my first post"
exampleComment = CommentC "a comment!"
examplePage    = PageC    "my first page"

parent   x = (x, User, DR,  Suc (Suc (Suc Zero)))
children x = (x, User, DL,  Suc (Suc (Suc Zero)))
homepageP x = (x, User, DL, Suc (Suc (Suc (Suc Zero))))

authorP x = (x, Post, DR, Zero)

-- example flow
--

example :: Basil Blog BlogEnv ERRelationsBlog Logger String
example = do homepage <- return $ Ref Page $ UID 666
             chris    <- new (exampleUser "chris") ((parent (Ref User (UID 999))) `PCons` (PCons (homepageP homepage) PNil))
             piet     <- new (exampleUser "piet") ((parent chris) `PCons` (PCons (homepageP homepage) PNil))
             post     <- new examplePost (PCons (authorP chris) PNil)
             auth     <- getRelation post (DR, Zero)
             auth     <- getRelation post (DR, Zero)
             lift $ log (fmap pKey auth)
             name     <- attr chris lName
             return name

-- to run
runIt = runLog $ runBasil $ example


-- Logger code

instance Monad (Logger Blog) where
  return x = Log (return x)
  (>>=) l r = Log (runLog l >>= (fmap runLog r))

newtype Logger (phi :: * -> *) a = Log {runLog :: IO a}

instance Functor (Logger Blog) where fmap f = Log . fmap f . runLog

log :: Show a => a -> Logger phi ()
log = Log . print

instance Persist Logger Blog where
  pFetch tix ix = do log ix
                     return Nothing

-- Boilerplate code (will be TH)
--
type BlogEnv = (User, (Post, (Comment, (Page, ()))))

instance EnumTypes Blog BlogEnv where
  allTypes      = WCons (WCons (WCons (WCons WNil)) )
  index User    = Zero
  index Post    = Suc Zero
  index Comment = Suc (Suc Zero)
  index Page    = Suc (Suc (Suc Zero))

-- TODO: this should be TH as well.
type instance TypeEq User     User    = True 
type instance TypeEq User     Post    = False
type instance TypeEq User     Comment = False
type instance TypeEq User     Page    = False
type instance TypeEq Post     User    = False
type instance TypeEq Post     Post    = True 
type instance TypeEq Post     Comment = False
type instance TypeEq Post     Page    = False
type instance TypeEq Comment  User    = False
type instance TypeEq Comment  Post    = False
type instance TypeEq Comment  Comment = True 
type instance TypeEq Comment  Page    = False
type instance TypeEq Page     User    = False
type instance TypeEq Page     Post    = False
type instance TypeEq Page     Comment = False
type instance TypeEq Page     Page    = True

-- TODO: this should be TH as well.
instance TEq Blog where
 tEq User     User    = TTrue 
 tEq User     Post    = TFalse
 tEq User     Comment = TFalse
 tEq User     Page    = TFalse
 tEq Post     User    = TFalse
 tEq Post     Post    = TTrue 
 tEq Post     Comment = TFalse
 tEq Post     Page    = TFalse
 tEq Comment  User    = TFalse
 tEq Comment  Post    = TFalse
 tEq Comment  Comment = TTrue 
 tEq Comment  Page    = TFalse
 tEq Page     User    = TFalse
 tEq Page     Post    = TFalse
 tEq Page     Comment = TFalse
 tEq Page     Page    = TTrue
