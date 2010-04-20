{-# LANGUAGE GADTs, KindSignatures, MultiParamTypeClasses, TypeFamilies, TypeOperators, TypeSynonymInstances, EmptyDataDecls, TemplateHaskell, FlexibleContexts, FlexibleInstances, FunctionalDependencies, ScopedTypeVariables, UndecidableInstances #-}

module CoreData2 where

import Basil hiding ((:*:))
import Basil.Data.TList.TH
import Data.Record.Label (mkLabels, label)
import Prelude hiding (log)
import Basil.Database.Relational.Interface
import Generics.Regular
import qualified Basil.Data.TList as T
import qualified Data.Set as S
import Control.Monad.Trans (lift)

data User    = UserC    {name :: String, password :: String, age :: Int} deriving (Show)
data Post    = PostC    {title :: String, body :: String} deriving (Show)
data Comment = CommentC {text :: String} deriving Show
data Tag     = TagC     {tag :: String} deriving Show

$(mkTList "Blog"  [''User, ''Post, ''Comment, ''Tag])

type TAuthorPosts    =  Rel Blog One  User  Many   Post
type TAuthorComments =  Rel Blog One  User  Many   Comment
type TPostComments   =  Rel Blog One  Post  Many   Comment

$(mkTList "BlogRelationsEnum" [''TAuthorPosts, ''TAuthorComments, ''TPostComments])

$(mkLabels [''User, ''Post, ''Comment, ''Tag])
$(deriveAll ''User "PFUser")
type instance PF User = PFUser
$(deriveAll ''Post "PFPost")
type instance PF Post = PFPost
$(deriveAll ''Comment "PFComment")
type instance PF Comment = PFComment
$(deriveAll ''Tag "PFTag")
type instance PF Tag = PFTag

exampleUser :: String -> User
exampleUser  n = UserC    n "test" 24
examplePost ::  Post
examplePost    = PostC    "fipo" "my first post"
exampleComment :: Comment
exampleComment = CommentC "a comment!"

-- example flow
--

example :: BasilDB Blog BlogRelationsEnum [(Ref Blog Post,Post)]
example = do createDatabase
             let ref = Ref ixUser 1
             user <- find ref
             lift $ print $ user
             -- new ixPost (examplePost {body = "hello, world"}) (PCons (authorP ref) PNil)
             Just rels <- findRels DL ixTAuthorPosts (Ref ixUser 1)
             mapM find $ S.toList rels
             findAll ixPost

-- boilerplate, will be generated using quasiquoting.

authorP :: Ref Blog User -> (Ref Blog User, Dir R, Ix BlogRelationsEnum TAuthorPosts)
authorP x = (x, DR, Zero)

-- age_ = Attribute "age" age


instance ERModel Blog BlogRelationsEnum where
  relations = TCons4 authorPosts 
            $ TCons4 authorComments 
            $ TCons4 postComments 
            $ TNil4
  witnesses = WCons Zero (WCons (Suc Zero) (WCons (Suc (Suc Zero)) (WCons (Suc (Suc (Suc Zero))) WNil)))


-- ixAuthorPosts :: Ix BlogRelationsEnum TAuthorPosts
-- ixAuthorPosts = Zero


authorPosts    :: TAuthorPosts
authorPosts    = mkRelation ("author", One, ixUser)   ("posts"   , Many, ixPost)

authorComments :: TAuthorComments
authorComments = mkRelation ("author", One, ixUser)   ("comments", Many, ixComment)
postComments   :: TPostComments
postComments   = mkRelation ("post"  , One, ixPost)   ("comments", Many, ixComment)

$(mkEqualities [''User, ''Post, ''Comment, ''Tag])
