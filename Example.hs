{-# LANGUAGE GADTs, KindSignatures, MultiParamTypeClasses, TypeFamilies, TypeOperators, TypeSynonymInstances, EmptyDataDecls, TemplateHaskell, FlexibleContexts, FlexibleInstances, FunctionalDependencies, ScopedTypeVariables, UndecidableInstances #-}

module CoreData2 where

import Data.Record.Label (mkLabels, label)
import Generics.MultiRec.Base hiding (Tag)
import Generics.MultiRec.TH
import Basil
import qualified Data.Map as M
import Prelude hiding (log)
import Basil.Data.TList4
import Control.Monad.Trans (lift)

data User = UserC {name :: String, password :: String, age :: Int} deriving (Show)
data Post = PostC {title :: String, body :: String} deriving (Show)
data Comment = CommentC {text :: String} deriving Show
data Tag     = TagC {tag :: String} deriving Show

type BlogEnum = (User, (Post, (Comment, (Tag, ()))))
data Blog :: * -> * where
  User    :: Blog User
  Post    :: Blog Post
  Comment :: Blog Comment
  Tag     :: Blog Tag

$(mkLabels [''User, ''Post, ''Comment, ''Tag])
$(deriveConstructors [''User, ''Post, ''Comment,  ''Tag])
$(deriveSystem ''Blog [''User, ''Post, ''Comment, ''Tag] "PFBlog")
type instance PF Blog = PFBlog

type BlogRelationsEnum = ((One `To` Many) User Post
                        ,((One `To` Many) User Comment
                        ,((One `To` Many) Post Comment 
                        ,((One `To` Many) User User
                        ,()
                        ))))

instance ERModel Blog BlogRelationsEnum where
  relations = TCons4 User    Post authorPosts 
            $ TCons4 User Comment authorComments 
            $ TCons4 Post Comment postComments 
            $ TCons4 User    User parentChildren
            $ TNil4


authorPosts    = mkRelation ("author", One, User)   ("posts"   , Many, Post)
authorComments = mkRelation ("author", One, User)   ("comments", Many, Comment)
postComments   = mkRelation ("post"  , One, Post)   ("comments", Many, Comment)
parentChildren = mkRelation ("parent", One, User)   ("children", Many, User)

type To m1 m2 t1 t2 = Rel Blog m1 t1 m2 t2


exampleUser  n = UserC    n "test" 24
examplePost    = PostC    "fipo" "my first post"
exampleComment = CommentC "a comment!"

parent   x = (x, User, DR,  Suc (Suc (Suc Zero)))
children x = (x, User, DL,  Suc (Suc (Suc Zero)))

authorP x = (x, Post, DR, Zero)

-- example flow
--

example :: Basil Blog BlogEnum BlogRelationsEnum Logger String
example = do chris    <- new (exampleUser "chris") ((parent (Ref User (UID 999))) `PCons` (PNil))
             piet     <- new (exampleUser "piet") ((parent chris) `PCons` (PNil))
             post     <- new examplePost (PCons (authorP chris) PNil)
             auth     <- getRelation post (DR, Zero)
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

-- Boilerplate code (will be TH eventually)

instance EnumTypes Blog BlogEnum where
  allTypes      = WCons (WCons (WCons (WCons WNil)))
  index User    = Zero
  index Post    = Suc Zero
  index Comment = Suc (Suc Zero)
  index Tag     = Suc (Suc (Suc Zero))

-- TODO: this should be TH as well.
type instance TypeEq User     User    = True 
type instance TypeEq User     Post    = False
type instance TypeEq User     Comment = False
type instance TypeEq User     Tag     = False
type instance TypeEq Post     User    = False
type instance TypeEq Post     Post    = True 
type instance TypeEq Post     Comment = False
type instance TypeEq Post     Tag     = False
type instance TypeEq Comment  User    = False
type instance TypeEq Comment  Post    = False
type instance TypeEq Comment  Comment = True 
type instance TypeEq Comment  Tag     = False
type instance TypeEq Tag      User    = False
type instance TypeEq Tag      Post    = False
type instance TypeEq Tag      Comment = False
type instance TypeEq Tag      Tag     = True

-- TODO: this should be TH as well.
instance TEq Blog where
 tEq User     User    = TTrue 
 tEq User     Post    = TFalse
 tEq User     Comment = TFalse
 tEq User     Tag     = TFalse
 tEq Post     User    = TFalse
 tEq Post     Post    = TTrue 
 tEq Post     Comment = TFalse
 tEq Post     Tag     = TFalse
 tEq Comment  User    = TFalse
 tEq Comment  Post    = TFalse
 tEq Comment  Comment = TTrue 
 tEq Comment  Tag     = TFalse
 tEq Tag      User    = TFalse
 tEq Tag      Post    = TFalse
 tEq Tag      Comment = TFalse
 tEq Tag      Tag     = TTrue
