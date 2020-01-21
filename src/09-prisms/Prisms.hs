{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
module Prisms where

-------------------------------------

import           Control.Applicative
import           Control.Applicative
import           Control.Lens        as L
import           Control.Monad.State
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as BS
import           Data.Char
import           Data.Function       (on)
import qualified Data.List           as List
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Monoid
import           Data.Ord            (comparing)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Tree
import           Text.Read           (readMaybe)
------------------------------------------------------
-- Prism

-- At most one focus.
--
-- Prism can run **backwards**, taking a focus and embedding it into a structure!
--
-- All prism are valid traversal


--              Lens         Fold         Traversal         Prism
-- Get          Single       Many         Many              Zero or One
-- Set/Modify   Single       Nope         Many              Zero or One
-- Traverse     Single       Nope         Many              Zero Or One
-- Embed        Single       Nope         Nope              One

------------------------------------------------------------

-- Simple Pattern-Matching Prisms

-- Each prism represents a possible pattern that a structure could match.

-- _Left  :: Prism (Either l r) (Either l' r) l l'
-- _Right :: Prism (Either l r) (Either l r') r r'

-- >>> Left "message" ^? _Left
-- Just "message"
-- >>> Left "message" ^? _Right
-- Nothing

-- >>> Right 42 ^? _Right
-- Just 42
-- >>> Right 42 ^? _Left
-- Nothing

-- Since prism are valid traversals, we can set, update or traverse
-- >>> Left 10 & _Left +~ 5
-- Left 15
-- >>> Right "howdy" & _Right %~ reverse


-- _Nothing :: Prism' (Maybe a) ()
-- _Just    :: Prism' (Maybe a) (Maybe b) a b
--
-- >>> Nothing ^? _Nothing
-- Just ()
-- >>> Nothing ^? _Nothing
-- Nothing
-- >>> Just 20 & _Just %~ (+ 10)
-- Just 30


------------------------------------------------------

-- Checking pattern matches with prisms

-- has   :: Fold s a      -> s -> Bool
-- isn't :: Prism s t a b -> s -> Bool
-- ^^^^^^^^^^^ has checks wheter the given fold yields any elements when run on teh provided structure. isn't the oppositve

-- >>> has _Right (Left "mesage")
-- False
-- >>> has _Right (Right 42)
-- True
-- >>> has _Nothing Nothing
-- True
-- >>> isn't _Left (Left ())
-- False
-- >>> isn't _Nothing (Just "hi")
-- True

-- has folded []
-- False


--------------------------------------------------------

-- Generating prisms with makePrisms

-- type Path = [String]
-- type Body = String
--
-- data Request =
--     Post Path Body
--   | Get Path
--   | Delete Path
--   deriving Show
-- makePrisms ''Request
-- >>> :browse Prisms
-- _Post :: Prism' Request (Path, Body)
-- _Get :: Prism' Request Path
-- _Delete :: Prism' Request Path

-- >>> Get ["users"] ^? _Get
-- Just ["users"]
-- >>> Get ["users"] ^? _Post
-- Nothing
-- >>> Post ["users"] "name: John" & _Post . _1 <>~ ["12345"]
-- Post ["users","12345"] "name: John"


------------------------------------------------------

-- Embedding values with prisms

-- > Use the constructor of the type when possible

-- review :: Prism s t a b -> b -> t
-- (#)    :: Prism s t a b -> b -> t

-- >>> review _Get ["posts"]
-- Get ["posts"]
-- >>> _Get # ["posts"]
-- Get ["posts"]
-- >>> _Post # (["posts"], "My blog post")
-- Post ["posts"] "My blog post"
-- >>> review _Left "an error"
-- Left "an error"
--
-- Notice how the cmposition associates
-- >>> _Just . _Left # 1337
-- Just (Left 1337)

------------------------------------------------------

-- Other types of patterns

-- class Cons s t a b | s -> a, t -> b, s b -> t, t a -> s where
--   _Cons :: Prism s t (a, s) (b, t)

-- Some implementations:
--   _Cons :: Prism [a]        [b]        (a, [a])             (b, [b])
--   _Cons :: Prism (Seq a)    (Seq b)    (a, Seq a)           (b, Seq b)
--   _Cons :: Prism (Vector a) (Vector b) (a, Vector a)        (b, Vector b)
--   _Cons :: Prism' String               (Char, String)
--   _Cons :: Prism' Text                 (Char, Text)
--   _Cons :: Prism' ByteString           (Word8, ByteString)


-- >>> [1,2,3] ^? _Cons
-- Just (1, [2, 3]_
-- >>> "Freedom!" ^? _Cons
-- Just ('F', "reedom!")
-- >>> "" ^? _Cons
-- Nothing
-- >>> ("Freedom!"::Text) & _Cons . _2 %~ T.reverse
-- "F!modeer"



-- >>> _Cons # ('F', "reedom")
-- "Freedom"
-- >>> review _Cons (65, "BC" :: ByteString)
-- "ABC"

---------------------------------------------------------

-- If all we want to do is access the head or tail safely, the Control.Lens.Cons module provides:
--
-- >>> "Hello" & _head .~ 'J'
-- Jello
-- >>> "Freedom! & _tail %~reverse
-- "F!modeer"
--
-- nb. _head and _tail are only Traversal and not Prism because there's no way for us to run them in reverse like.


-- class AsEmpty a where
--   _Empty :: Prism' a ()

-- >>> isn't _Empty []
-- False
-- >>> isn't _Empty [1,2,3]
-- True
-- >>> has _Empty Map.empty
-- True
is = has -- reads better
-- >>> is _Empty (Set.fromList [1,2,3])
-- False



-- _Show :: (Read a, Show a) => Prism' String a
-- ^^^ ---> Reads
-- ^^^ <--- Shows

-- >>> "12" ^? _Show :: Maybe Int
-- Just 12
-- >>> "12" ^? _Show :: Maybe Bool
-- Nothing
-- >>> "It's True that I ate 3 apples and 5 oranges" ^.. worded . _Show :: [Int]
-- [3, 5]
-- >>> "It's True that I ate 3 apples and 5 oranges" ^.. worded . _Show :: [Bool]
-- [True]
-- review _Show (Just 12 :: Maybe Int)
-- "Just 12"

----------------------------------------------------------------

-- Exercises - Prisms

-- 1.

data ContactInfo =
    Email String
  | Telephone Int
  | Address String String String

makePrisms ''ContactInfo
-- _Email :: Prism' ContactInfo String
-- _Telephone :: Prism' ContactInfo Int
-- _Address :: Prism' ContactInfo String String String


-- 2.

-- >>> Right 35 & _Right +~ 5
-- Right 40
--
-- >>> [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"] ^.. folded . _Just
-- ["Mind","Power","Soul","Time"]
--
-- >>> [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"] &  traversed . _Just <>~ " Stone"
-- [Just "Mind Stone",Just "Power Stone",Nothing,Just "Soul Stone",Nothing,Just "Time Stone"]
--
-- >>> Left (Right True, "Eureka!") & _Left . _1 . _Right %~ not
-- Left (Right False,"Eureka!")
--
-- >>> _Cons # ("Do", ["Re", "Mi"])
-- ["Do", "Re", "Mi"]
--
-- >>> isn't (_Show @Int) "not an int"
-- True

-- 3.

-- >>> (Just 1, Nothing, Just 3) ^.. each . _Just
-- [1,3]
--
-- >>> review (backwards _Cons) ('x', "yz")
-- >>> ('x', "yz") & review _Cons
--                 & _tail %~ reverse
-- "xyz"
--
-- >>> _Left . _Just . _Right # "do the hokey pokey"
-- Left (Just (Right "do the hokey pokey"))


------------------------------------------------------

-- Writing Custom Prisms

-- we can build a prism for any pattern which can be reversed.

--            embed            match
-- prism  :: (b -> t) -> (s -> Either t a) -> Prism s t a b
-- prims' :: (b -> s) -> (s -> Maybe    a) -> Prism s s a b


_Just' :: Prism (Maybe a) (Maybe b) a b
_Just' = prism embed match
  where
    match :: Maybe a -> Either (Maybe b) a
    match (Just a) = Right a
    match Nothing = Left Nothing
    embed :: b -> Maybe b
    embed b = Just b

_Nothing' :: Prism' (Maybe a) ()
_Nothing' = prism' embed match
  where
    match :: Maybe a -> Maybe ()
    match Nothing = Just ()
    match (Just _) = Nothing
    embed :: () -> Maybe a
    embed () = Nothing


----------------------------------------------------

-- Matching Strng Prefixes

-- >>> import Data.List (stripPrefix)
_Prefix :: String -> Prism' String String
_Prefix prefix = prism' embed match
  where
    match :: String -> Maybe String
    match s = List.stripPrefix prefix s
    embed :: String -> String
    embed s = prefix <> s

-- >>> "http://phishingscam.com" ^? _Prefix "https://"
-- Nithing
-- >>> "https://totallylegit.com" ^? _Prefix "https://"
-- Just "totallylegit.com"

-- >>> let _Secure = _Prefix "https://"
-- >>> "https://mybank.com" & _Secure <>~ "?accountNumber=12345"
-- "https://mybank.com?accountNumber=12345"
-- >>> "http://fakebank.com" & _Secure <>~ "?accountNumber=12345"
-- "http://fakebank.com"


----------------------------------------------------

-- Cracking the coding interview: Prisms style!

-- Implement FizzBuzz only with prims:
--
-- Matches only the multiples of the given number
_Factor :: Int -> Prism' Int Int
_Factor n = prism' embed match
  where
    embed :: Int -> Int
    embed i = i * n
    match :: Int -> Maybe Int
    match i
      | i `mod` n == 0 = Just (i `div` n)
      | otherwise = Nothing

-- >>> has (_Factor 3) 9
-- True
-- >>> has (_Factor 7) 9
-- False
-- >>> 15 ^? _Factor 3 . _Factor 5
-- Just 1

prismFizzBuzz :: Int -> String
prismFizzBuzz n
  | has (_Factor 3 . _Factor 5) n = "FizzBuzz"
  | has (_Factor 3) n = "Fizz"
  | has (_Factor 5) n = "Buzz"
  | otherwise = show n

runFizzBuzz :: IO ()
runFizzBuzz = traverseOf_ traversed (putStrLn . prismFizzBuzz) [1..20]
-- >>> runFizzBuzz
-- 1
-- 2
-- Fizz
-- 4
-- Buzz

------------------------------------------------------------

-- Exercises - Custom Prisms

-- 1.
_Tail :: Prism' [a] [a]
_Tail = prism' embed match
  where
    embed :: [a] -> [a]
    embed = undefined
    match :: [a] -> Maybe [a]
    match = Just . tail
-- ^^^^^^^^^^^ It's not possible because we lost information when we tailed.

-- 2.
_ListCons :: Prism [a] [b] (a, [a]) (b, [b])
_ListCons = prism embed match
  where
    embed :: (b, [b]) -> [b]
    embed (h, t) = h : t
    match :: [a] -> Either [b] (a, [a])
    match []     = Left []
    match (x:xs) = Right (x, xs)
-- >>> [1,2,3,4] ^? _ListCons
-- Just (1, [2,3,4])
-- >>> [1,2,3,4] & _ListCons . _2 . traversed +~ 1
-- [1,3,4,5]
-- >>> [] & _ListCons . _2 . traversed +~ 1
-- []

-- 3.
_Cycles :: (Eq a) => Int -> Prism' [a] [a]
_Cycles n = prism' embed match
  where
    embed :: [a] -> [a]
    embed prefix = concat $ replicate n prefix -- prefix ^. taking n repeated
    match :: (Eq a) => [a] -> Maybe [a]
    match xs = let prefix = take (length xs `div` n) xs
               in if (concat $ replicate n prefix) == xs
                    then Just prefix
                    else Nothing
              -- has (only xs) (prefix ^. taking n repeated)

-- >>> "dogdogdog" ^? _Cycles 3
-- Just "dog"
-- >>> "dogdogdogdog" ^? _Cycles 4
-- Nothing
-- >>> _Cycles 3 # "dog"
-- "dogdogdog"
-- >>> "dogdogdog" & _Cycles 3 .~ "cats"
-- "catscatscats"


--------------------------------------------------

-- Laws

-- TL;DR: prism should behave like a reversible pattern match.

-- 1) review-preview
--   * preview p (review p value) == Just value

-- >>> preview _Left (review _Left "Habber") == Just "Habber"
-- True
-- >>> let cycles3 = _Cycles 3
--     in preview cycles3 (review cycles3 "dog") == Just "dog"
-- True

-- 2) prism complement:
--     let Just a = preview myPrism s
--              s'= review myPrism a
--     in s == s'

-- >>> let s = "[1, 2, 3]"
-- >>> let Just a = preview (_Show :: Prism' String [Int]) s
-- >>> let s' = review _Show a
-- >>> s == s'   -- whitespaces are removed
-- False

-- 3) pass-through reversion

-- TL;DR "If the prism fails to match and we type cast the structure into a new type; that we can use the same prism to type cast it back into its original type."

-- prisms allow us tochange the type parameter in the case when we *don't* match. We can clearly witness this using the matching combinator:
--
-- matching :: Prism s t a b -> s -> Either t a
--
-- Either we  can match on the prism and therefore have access to the focus, *or* the prism doesn't match, and therefore we can change the type of our structure.

-- >>> :t matching _Just
-- matching _Just :: Maybe a -> Either (Maybe b) a

-- The law states:
--  "If the prism fails to match and we type cast the structure into a new type; that we can use the same prism to type cast it back into its original type."

-- >>> let Left t  = matching l s
-- >>> let Left s' = matching l t
-- s == s'

-- >>> let s = Nothing :: Maybe Int
-- >>> let Left (t :: Maybe String) = matching _Just s
-- >>> let Left (s' :: Maybe Int) = matching _Just t
-- >>> s == s'

-----------------------

-- Exercises - Prism Laws

-- 1.

-- Is it lawful ?
-- Law 1: ko
--   preview (_Contains 3) (review (_Contains 3) (Set.fromList [1,2,3])) == Just ( Set.fromList [1,2,3] )
--   False

_Contains :: forall a. Ord a => a -> Prism' (Set a) (Set a)
_Contains a = prism' embed match
  where
    embed :: Set a -> Set a
    embed = Set.insert a
    match :: Set a -> Maybe (Set a)
    match s = if elemOf folded a s
                then Just (Set.delete a s)
                else Nothing

-- >>> Set.fromList [1,2,3] ^? _Contains 2
-- Just (fromList [1,3])
-- >>> Set.fromList [1,2,3] ^? _Contains 10
-- Nothing
-- >>> _Contains 10 # Set.fromList [1,2,3]
-- Set.fromList [1,2,3,10]
-- >>> _Contains 2 # Set.fromList [1,2,3]
-- Set.fromList [1,2,3]


-- 2.

_Singleton :: forall a. Prism' [a] a
_Singleton = prism' embed match
  where
    match :: [a] -> Maybe a
    match [a] = Just a
    match _ = Nothing
    embed :: a -> [a]
    embed a = [a]
-- Law 1)
-- >>> preview _Singleton (review _Singleton [1,2,3]) == Just [1,2,3]
-- >>> preview _Singleton (review _Singleton [1]) == Just [1]
-- >>> preview _Singleton (review _Singleton []) == Just []
-- Law 2)
-- ...

-- 3. Write your own prism which fails the first law!



-------------------------------------------------------

-- Case Study: Simple Server

type Path = [String]
type Body = String

data Request =
    Post Path Body
  | Get Path
  | Delete Path
  deriving Show
makePrisms ''Request

path :: Lens' Request Path
path = lens getter setter
  where
    getter (Post p body) = p
    getter (Get p)       = p
    getter (Delete p)    = p
    setter (Post _ body) p = Post p body
    setter (Get _) p       = Get p
    setter (Delete _) p    = Delete p
-- >>> Get ["posts", "12345"] ^. path
-- >>> Post ["posts", "12345"] "My new post" & path .~ ["hello"]

serveRequest :: Request -> String
serveRequest _ = "404 Not Found"

-- >>> serveRequest (Get ["hello"])

-- Path prefixes and serving different handlers on each route.

_PathPrefix :: String -> Prism' Request Request
_PathPrefix prefix = prism' embed match
  where
    embed :: Request -> Request
    embed req = req & path %~ (prefix :)
    match :: Request -> Maybe Request
    match req
      | has (path . _head . only prefix) req = Just (req & path %~ drop 1)
    match _ = Nothing

-- >>> (Get ["users", "all"]) ^? _PathPrefix "users"
-- Just (Get ["all"])
-- >>> (Delete ["posts", "12345"]) ^? _PathPrefix "posts"
-- Just (Delete ["12345"])
-- >>> _PathPrefix "posts" # Delete ["12345"]
-- Delete ["posts","12345"]

-- > We could have implemented _PathPrefix much easier `prefix` prism.
--
-- >>> import Data.List.Lens
-- prefixed :: Eq a => [a] -> Prism' [a] [a]


------------------------------------------

-- ****
-- oustide :: Prism s t a b -> Lens (t -> r) (s -> r) (b -> r) (a -> r)
-- ****


-- >>> tail []
-- *** Exception ..
--
-- We want to change the behaviour of the function `tail` on the empty list case.

-- _Empty :: Prism' [a] ()
-- oustide _Empty :: Lens' ([a] -> [a]) (() -> [a])
safeTail :: [a] -> [a]
safeTail = tail & outside _Empty .~ const []
             -- & outside _Cons .~ snd

-- >>> safeTail []
-- []


------------

-- Altering sub-sets of functions

userHandler :: Request -> String
userHandler req =
  "User handler! Remaining path: " <> List.intercalate "/" (req ^. path)

-- postsHandler :: Request -> String
-- postsHandler =
--   const "Posts Handler!"
--     & outside (_PathPrefix "index") .~ const "Post Index"

server :: Request -> String
server = serveRequest
          & outside (_PathPrefix "users") .~ userHandler
          & outside (_PathPrefix "posts") .~ postsHandler

-- >>> server (Get ["users", "id", "12345"])
-- "User handler! Remaining path: id/12345"
-- >>> server (Post ["admin"] "DROP TABLE users")
-- "404 Not Found"
-- >>> server (Delete ["posts", "id", "12345"])
-- "Post Handler!"
-- >>> server (Delete ["posts", "12345"])
-- "Posts Handler!"


-------------


-- Matching on HTTP Verb

-- _Post :: Prism' Request (Path, Body)
-- _Get :: Prism' Request Path
-- _Delete :: Prism' Request Path


-- >>> outside (_PathPrefix "posts" . _Post)
--  :: Lens (Request -> String) ((Path, Body) -> String)

postsHandler :: Request -> String
postsHandler = const "Posts Handler!"
  & outside _Post
      .~ (\(path', body) -> "Created post with body: " <> body)
  & outside _Get
      .~ (\path' -> "Fetching post at path: " <> List.intercalate "/" path')
  & outside _Delete
      .~ (\path' -> "Deleting post at path: " <> List.intercalate "/" path')


-- >>> server (Get ["posts", "12345"])
-- >>> server (Post ["posts", "12345"] "My new post")
-- >>> server (Delete ["posts", "12345"])

-- Plain haskell
postsHandler' :: Request -> String
postsHandler' (Post path' body) = "Created post with body: " <> body
postsHandler' (Get path')       = "Fetching post at path: " <> intercalate "/" path'
postsHandler' (Delete path')    = "Deleting post at path: " <> intercalate "/" path'

-- This standard code is:
--   * much more readable
--   * easier to understand
--   * doesn’t need a “default case”
--   * and will automatically be checked for completeness if you’ve told GHC to check for incomplete pattern matches.
--
-- So when should you use prisms?

-- 1. Prisms are composable: doing multiple pattern matches in standard Haskell requires nesting function calls or case expressions
--
-- 2. Prisms interoperate with the rest of optics, providing a lot of flexibility and expressiveness. You can perform pattern matching inside of an optics chain!
