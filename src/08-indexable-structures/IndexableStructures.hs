{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE InstanceSigs        #-}
module IndexableStructures where

-------------------------------------

import           Control.Applicative
import           Control.Applicative
import           Control.Lens        as L
import           Control.Monad.State
import           Data.Char
import           Data.Function       (on)
import           Data.Map (Map)
import qualified Data.Map            as Map
import           Data.Monoid
import           Data.Ord            (comparing)
import qualified Data.Set            as Set
import           Data.Text (Text)
import qualified Data.Text           as T
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Tree
import           Text.Read           (readMaybe)

-------------------------------------

-- Indexed data structures: lists (offset), maps (key), rose tree (list of integers)
--
-- class Ixed m where
--   ix :: Index m -> Traversal' m (IxValue m)
--   ^^^^^^ Index, IxValue are both type families.
--
-- Given an index, builds a traversal to that index.
--
-- type instance Index [a] = Int
-- type instance Index (Map k a) = k
-- type instance Index ByteString = Int
-- type instance Index Text = Int
--
-- type instance IxValue [a] = a
-- type instance IxValue (Map k a) = a
-- type instance IxValue Text = Char
-- type instance IxValue ByteString = Word8
--
-- ^^^^^ those already have instances but sometimes you need to define them.

--------------

-- Accessing and setting values with ix

-- >>> ["Borg", "Cardassian", "Talaxian"] ^? ix 1
-- Just "Cardassian"
-- >>> ["Borg", "Cardassian", "Talaxian"] & ix 1 .~ "Vulcan"
-- ["Borg","Vulcan","Talaxian"]
--
-- nb. Set on missing elements doesn't work (it would break second Traversal law)
-- >>> ["Borg", "Cardassian", "Talaxian"] & ix 4 .~ "Vulcan"
-- ["Borg","Cardassian","Talaxian"]
--
-- let benders = Map.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")]
--
-- >>> benders ^? ix "Zuko"
-- Just "Fire"
--
-- -- doesn't insert the new key-value
-- >>> benders & ix "Iroh" .~ "Lightning"


---------------

-- Indexed Structures

-- Structure |  Index  | Value
-- ---------------------------------
-- [a]           Int      a
-- NonEmpty a    Int      a
-- Seq a         Int      a
-- Vector a      Int      a
-- Set a         a        ()
-- Map k a       k        a
-- Identity a    ()       a
-- Maybe a       ()       a
-- Tree a        [Int]    a
-- Text a        Int      Char
-- ByteString a  Int      Word8
-- (e -> a)      e        a


----------------

-- Indexing monomorphic types

-- >>> ("hello" :: T.Text) ^? ix 0
-- Just 'h'
-- >>> ("hello" :: BS.ByteString) & ix 0 +~ 2
-- "jello"
-- >>> ("hello" :: T.Text) & ix 1 %%~ (\_ -> ("aeiou" :: [Char]))
-- ["hallo","hello","hillo","hollo","hullo"]


----------------

-- Indexing stranger structures

tree :: Tree Int
tree = Node 1 [ Node 2 [Node 4 [] ]
              , Node 3 [Node 5 [], Node 6 []]
              ]

-- >>> tree ^? ix []
-- 1
-- >>> tree ^? ix [0]
-- 2
-- >>> tree ^? ix [0, 0]
-- 4
-- >>> tree & ix [0, 0] .~ 0

-- Indexing functions (not recommended, just to show off)

-- >>> reverse ^? ix "Stella!"
-- Just "!alletS"

-- >>> let specialReverse = reverse & ix "password" .~ "You found the secret!"
-- >>> specialReverse "Stella!"
-- "!alletS"
-- >>> specialReverse "password"
-- "You found the secret!"


----------------

-- Inserting & Deleting with `at`

-- The At typeclass allows focusing values within map-like structures which allow arbitrary insertion or deletion.
--
-- class At where
--   at :: Index m -> Lens' m (Maybe (IxValue m))
--
-- To insert or replace an element we can set a value wrapped in Just; to delete a we can set the focus to Nothing.
--
-- Unfortunately, we can't write an At instance for lists since we can't insert elements arbitrarily.
--
-- >>> let benders = Map.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")]
-- >>> benders ^. at "Zuko"
-- Just "Fire"
-- >>> benders & at "Zuko" .~ Nothing
-- fromList [("Katara","Water"),("Toph","Earth")]
-- >>> benders & at "Iroh" .~ Just "Lightning"
-- fromList [("Iroh","Lightning"),("Katara","Water"),("Toph","Earth"),("Zuko","Fire")]


-- Because inserting new values using:
--
-- at i .~ Just newValue
--
-- is a very common pattern, the lens library provides a helper called (?~) which wraps values in Just before setting the value:
--
-- (?~) :: Traversal s t a (Maybe b) -> b -> s -> t
--
-- >>> benders & at "Iroh" ?~ "Lightning"
-- >>> (1,2) & both ?~ "twins!"
-- (Just "twins!", Just "twins!")


-- Deletion is a common operation too! The lens library provides `sans` as a helper to easily delete elements:
--
-- sans :: At m => Index m -> m -> m
-- sans k = at k .~ Nothing

-- >>> sans "Katara" benders
-- fromList [("Toph","Earth"),("Zuko","Fire")]


------------------------------

-- Manipulating Sets

-- One way to image a Set is a map where the set elements are keys: Map v ()
-- This is why :kind! IxValue (Set.Set a) == ()

-- >>> let primes = Set.fromList [2,3,5,7,11,13]
-- >>> primes ^? ix 5
-- Just () -- Exists
-- >>> primes & at 17 ?~ ()
-- fromList [2,3,5,7,11,13,17]
-- >>> primes & sans 5
--            & sans 7
-- fromList [2,3,11,13]


---------------------------

-- Exercises - Indexables Structures

-- 1.
-- >>> ["Larry", "Curly", "Moe"] & ix 1 .~ "Wiggly"
-- ["Larry","Wiggly","Moe"]
--
-- >>> let heroesAndVillains = Map.fromList [("Superman", "Lex"), ("Batman", "Joker")]
-- >>> heroesAndVillains & at "Spiderman" .~ Just "Goblin"
-- M.fromList [("Batman","Joker"),("Spiderman","Goblin"),("Superman","Lex")]
--
-- >>> sans "Superman" heroesAndVillains
-- Map.fromList [("Batman", "Joker")]
--
-- >>> Set.fromList ['a', 'e', 'i', 'o', 'u']
--       & at 'y' ?~ ()
--       & sans 'i'
-- S.fromList "aeouy"


-- 2. Use ix and at to go from the input to the output

-- >>> let input = Map.fromList [("candy bars", 13), ("soda", 34), ("gum", 7)]
--  >>> input
--        & ix "soda" +~ 3
--        & at "ice cream" ?~ 5     -- & at "ice cream" .~  Just 5
-- Map.fromList [("candy bars",13),("ice cream",5),("soda",37)]


----------------------------

-- Custom Indexed Data Structures

-- Newtype over list which handles indexing as though the list were infinitely cycled.

newtype Cycled a = Cycled [a]
  deriving Show

-- :set -XTypeFamilies
type instance Index (Cycled a) = Int
type instance IxValue (Cycled a) = a

-- :set -XInstanceSigs
instance Ixed (Cycled a) where
  -- ix :: Index m -> Traversal' m (IxValue m)
  -- ix :: (Applicative f) => Int -> (a -> f a) -> Cycled a -> f (Cycled a)
  ix :: Int -> Traversal' (Cycled a) a
  ix i handler (Cycled xs) =
   Cycled <$> traverseOf (ix (i `mod` length xs)) handler xs

-- >>> Cycled ['a', 'b', 'c'] ^? ix 1
-- Just 'b'
-- >>> Cycled ['a', 'b', 'c'] ^? ix 3
-- Just 'a'
-- >>> Cycled ['a', 'b', 'c'] ^? ix (-1)
-- Just 'c'
-- >>> Cycled ['a', 'b', 'c'] & ix 0 .~ '!'
-- Cycled "!bc"
-- >>> Cycled ['a', 'b', 'c'] & ix 10 .~ '!'
-- Cycled "a!c"


--------------------

-- Custom At: Address indexing

data Address =
  Address { _buildingNumber  :: Maybe String
          , _streetName      :: Maybe String
          , _apartmentNumber :: Maybe String
          , _postalCode      :: Maybe String
          } deriving Show
makeLenses ''Address

data AddressPiece =
    BuildingNumber
  | StreetName
  | ApartmentNumber
  | PostalCode
  deriving Show

type instance Index Address = AddressPiece
type instance IxValue Address = String

-- Now we can implement At.
-- Ixed is a superclass of At.
-- Lucky for us, there's a default implementation of Ixed in terms of At.

-- x :: (Applicative f, At m) => Index m -> LensLike' f m (IxValue m)
-- ix = ixAt
instance Ixed Address

instance At Address where
  at :: AddressPiece -> Lens' Address (Maybe String)
  at BuildingNumber  = buildingNumber
  at StreetName      = streetName
  at ApartmentNumber = apartmentNumber
  at PostalCode      = postalCode

-- >>> let addr = Address Nothing Nothing Nothing Nothing
-- >>> let sherlockAddr = addr & at StreetName      ?~ "Baker St."
--                             & at ApartmentNumber ?~ "221B"
-- >>> sherlockAddr & ix ApartmentNumber .~ "221A"
-- >>> sherlockAddr & sans StreetName

------------------------------

-- Exercises - Custom Indexed Structures

-- 1. Newtype around Map which makes indexing case insensitive (i.e. if the key is "one", both "One" and "one" must work)
--    Write the ix instance manually even though it has a default implementation.

newtype CaseInsensitive v = CaseInsensitive (Map.Map String v)
  deriving Show

type instance Index (CaseInsensitive v) = String
type instance IxValue (CaseInsensitive v) = v

instance Ixed (CaseInsensitive v) where
  ix :: (Applicative f) => String -> (v -> f v) -> CaseInsensitive v -> f (CaseInsensitive v)
  ix key handler (CaseInsensitive map1) = CaseInsensitive <$>
    traverseOf (ix (fmap toLower key)) handler map1

instance At (CaseInsensitive v) where
  at :: String -> Lens' (CaseInsensitive v) (Maybe v)
  at key handler (CaseInsensitive map1) = CaseInsensitive <$>
    traverseOf (at (map toLower key)) handler map1

-- >>> menu :: CaseInsensitive String; menu = CaseInsensitive $ Map.fromList [("one", "curry"), ("two", "samosa")]
-- >>> menu ^? ix "one"
-- >>> menu ^? ix "One"
-- >>> menu & sans "Two"
--          & at "One" ?~ "Samosa"
