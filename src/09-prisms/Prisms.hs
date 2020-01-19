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
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Monoid
import           Data.Ord            (comparing)
import qualified Data.Set            as Set
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Tree
import           Text.Read           (readMaybe)

-------------------------------------
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

------------------------

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


---------------------------

-- Checking pattern matches with prisms

-- has   :: Fold s a      -> s -> Bool
-- isn't :: Prism s t a b -> s -> Bool

-- has checks wheter the given fold yields any elements when run on teh provided structure. isn't the oppositve

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


------------------------------

-- Generating prisms with makePrisms

type Path = [String]
type Body = String

data Request =
    Post Path Body
  | Get Path
  | Delete Path
  deriving Show
makePrisms ''Request
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


---------------------------------

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

--------------------------------

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

---------------------------------

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



































