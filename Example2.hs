{-# LANGUAGE GADTs, KindSignatures, MultiParamTypeClasses, TypeFamilies, TypeOperators, TypeSynonymInstances, EmptyDataDecls, TemplateHaskell, FlexibleContexts, FlexibleInstances, FunctionalDependencies, ScopedTypeVariables, UndecidableInstances #-}

module CoreData2 where

import Basil
import Basil.Relations.PList
import Basil.Query
import Data.Record.Label (mkLabels, label)
import Prelude hiding (log)
import qualified Basil.InMemory.Relations.Storage as B
import qualified Basil.InMemory.Relations as X
import qualified Basil.Relations.InitialValues as I
import qualified Data.Map as M

data User = UserC {name :: String, password :: String, age :: Int} deriving (Show)
data Post = PostC {title :: String, body :: String} deriving (Show)
data Comment = CommentC {text :: String} deriving Show
data Tag     = TagC {tag :: String} deriving Show

type Blog = User :*: Post :*: Comment :*: Tag :*: Nil

ixUser    = Zero
ixPost    = Suc (Zero)
ixComment = Suc (Suc Zero)
ixTag     = Suc (Suc (Suc Zero))

-- data Blog :: * -> * where
--   User    :: Blog User
--   Post    :: Blog Post
--   Comment :: Blog Comment
--   Tag     :: Blog Tag

$(mkLabels [''User, ''Post, ''Comment, ''Tag])

type BlogRelationsEnum = ((One `To` Many) User Post
                        ,((One `To` Many) User Comment
                        ,((One `To` Many) Post Comment 
                        ,((One `To` Many) User User
                        ,()
                        ))))

instance ERModel Blog BlogRelationsEnum where
  relations = TCons4 ixUser    ixPost authorPosts 
            $ TCons4 ixUser ixComment authorComments 
            $ TCons4 ixPost ixComment postComments 
            $ TCons4 ixUser    ixUser parentChildren
            $ TNil4


authorPosts    = mkRelation ("author", One, ixUser)   ("posts"   , Many, ixPost)
authorComments = mkRelation ("author", One, ixUser)   ("comments", Many, ixComment)
postComments   = mkRelation ("post"  , One, ixPost)   ("comments", Many, ixComment)
parentChildren = mkRelation ("parent", One, ixUser)   ("children", Many, ixUser)

type To m1 m2 t1 t2 = Rel Blog m1 t1 m2 t2


exampleUser  n = UserC    n "test" 24
examplePost    = PostC    "fipo" "my first post"
exampleComment = CommentC "a comment!"

parent   x = (x, DR,  Suc (Suc (Suc Zero)))
children x = (x, DL,  Suc (Suc (Suc Zero)))

authorP x = (x, DR, Zero)

age_ = Attribute "age" age

-- example flow
--

example :: Basil Blog BlogRelationsEnum [(Ref Blog User, User)]
example = do chris    <- new (exampleUser "chris") ((parent (Ref ixUser (UID 999))) `PCons` (PNil))
             piet     <- new (exampleUser "piet") ((parent chris) `PCons` (PNil))
             post     <- new examplePost (PCons (authorP chris) PNil)
             --auth     <- getRelation post (DR, Zero)
             age      <- attr chris lAge
             drinking <- query (Not (age_ .<. Constant 18))
             return drinking

-- to run
runIt = runBasil $ example

-- Boilerplate code (will be TH eventually)

-- instance EnumTypes Blog BlogEnum where
--   allTypes      = WCons (WCons (WCons (WCons WNil)))
--   index User    = Zero
--   index Post    = Suc Zero
--   index Comment = Suc (Suc Zero)
--   index Tag     = Suc (Suc (Suc Zero))

-- testing
test = X.setValue (Ref ixUser (Fresh 1)) ((Ref ixPost (Fresh 2)), DL, Zero)
     $ test'
test' = 
       X.setValue (Ref ixUser (Fresh 1)) ((Ref ixPost (Fresh 3)), DL, Zero)
     $ B.empty (relations :: TList4 Rel Blog BlogRelationsEnum)
