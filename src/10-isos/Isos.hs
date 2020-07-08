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
module Isos where

-------------------------------------

import           Control.Applicative
import           Control.Applicative
import           Control.Lens        as L
import           Control.Monad.State
import           Data.Char           as Char
import           Data.Function       (on)
import qualified Data.List           as List
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty  as NEL
import           Data.Map (Map)
import qualified Data.Map            as Map
import           Data.Monoid
import           Data.Ord            (comparing)
import qualified Data.Set            as Set
import           Data.Text (Text)
import qualified Data.Text           as T
import           Data.Text.Lens
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Tree
import           Text.Read           (readMaybe)

-------------------------------------

-- Isos

-- Completly reversible without losing any data.

------------------------

-- Building Isos

-- iso :: (s -> a) -> (b -> t) -> Iso s t a b

packed' :: Iso' String Text
packed' = iso to' from'
  where
    to' :: String -> Text
    to' = T.pack
    from' :: Text -> String
    from' = T.unpack

-- We can use it like a lens

-- >>> ("Ay, caramba!" :: String) ^. packed'
-- "Ay, caramba!" :: Text

-- We can use it like a prism

-- >>> packed # ("Sufferin' Succotash" :: Text) -- review


-----------------

-- Flipping isos with from

-- from :: Iso s t a b -> Iso b a t s
--
-- from :: Iso' s a -> Iso' a s


-- >>> ("Good grief" :: String) ^. packed
-- >>> ("Good grief" :: Text) ^. from packed

unpacked' :: Iso' Text String
unpacked' = from packed'

-- > Both packed and unpacked are available at Data.Text.Lens

---------------------------

-- Modification under isomorphism


-- Convert the data through the iso, run the modification, then convert it back.


-- >>> let str = "Idol on a pedestal" :: String
-- >>> str & packed %~ T.replace "Idol" "Sand"
-- "Sand on a pedestal" :: String

-- >>> let txt = "Lore ipsum" :: Text
-- >>> txt & from packed . traversed %~ toUpper
-- "LORE IPSUM" :: Text

-----------------------------


-- Varieties of isomorphisms

-- * converting between encodings
-- * text formats
-- * strict <-> lazy representations
-- * data structures with different performance: List <-> Vector
-- * more


reversed' :: Iso' [a] [a]
reversed' = iso reverse reverse
-- >>> "hola" & reversed %~ ('!' :)
-- "hola!"


-- Helper to build isos where a function is its own inverse:
--
-- involuted :: (a -> a) -> Iso' a a
-- involuted f = iso f f


reversed'' :: Iso' [a] [a]
reversed'' = involuted reverse

-- >>> [1,2,3] & reversed'' %~ drop 1
-- [1,2]


-- We gain a lof of power combining isos with all the other combinators we've learned.

-- >>> [1,2,3,4] ^.. reversed . takingWhile (> 2) traversed
-- [4,3]
-- >>> "Blue suede shoes" & reversed . taking 1 worded . reversed .~ "gloves"
-- "Blue suede gloves"
-- Note how the traverse works on the focus throgh the way back
-- -- >>> "Blue suede shoes" & reversed . taking 1 worded .~ "gloves"
-- "Blue suede sevolg"


----------------

-- Rearranges

-- swapped :: Iso' (a, b) (b, a)

-- >>> ("Fall", "Pride") ^. swapped
-- ("Pride", "Fall")


-- *real signature*
-- swapped :: Swapped p => Iso (p a b) (p c d) (p b a) (p d c)

-- >>> Right "field" ^. swapped
-- Left "field"


-------------------

-- flipped :: Iso' (a -> b -> c) (b -> a -> c)

-- >>> let (++?) = (++) ^. flipped
-- >>> "A" ++? "B"
-- "BA"

------------------

-- curried   :: Iso' ((a, b) -> c) (a -> b -> c)
-- uncurried :: Iso' (a -> b -> c) ((a, b) -> c)

-- >>> let addTuple = (+) ^. uncurried
-- >>> addTuple (1,2)
-- 3

-----------------

-- >>> import Numeric.Lens

-- >>> 10 ^. negated
-- -10
-- >>> over negated (+10) 30
-- 20
-- >>> 100 ^. adding 50
-- 150
-- >>> 100.0 ^. dividing 10
-- 10.0

---------------------------------------------

-- Composing isos

-- >>> let txt = "Winter is coming" :: Text
-- >>> txt ^. unpacked . reversed
-- "gnimoc si retniW" :: String

-- >>> txt & unpacked . reversed %~ takeWhile (not . Char.isSpace)
-- "coming" :: Text


-- The transformation steps are:
--     ("Winter is coming" :: Text)    --> unpacked
-- --> ("Winter is coming" :: String)  --> reversed
-- --> ("gnimoc si retniW" :: String)  --> takeWhile (not . isSpace)
-- --> ("gnimoc" :: String)            --> from reversed
-- --> ("coming" :: String)            --> from unpacked
-- --> ("coming" :: Text)


-- >>> 30 & dividing 10 . multiplying 2 +~ 1
-- 35

--     30    --> diving 10
-- --> 3     --> multiplying 2
-- --> 6     --> +~ 1
-- --> 7     --> from (multiplying 2)
-- --> 3.5   --> from (dividing 10)
-- --> 35


-----------------------------------------------

-- Exercises - Intro to Isos

-- 1.

-- • Focus a Celsius temperature in Fahrenheit
-- Iso
-- • Focus the last element of a list
-- Traversal
-- • View a JSON object as its corresponding Haskell Record
-- Prism
-- • Rotate the elements of a three-tuple one to the right
-- Iso
-- • Focus on the ‘bits’ of an Int as Bools.
-- Traversal
-- • Focusing an IntSet from a Set Int
-- Iso

-- 2.

-- >>> ("Beauty", "Age") ^. swapped
-- ("Age", "Beauty")
--
-- >>> 50 ^. from (adding 10)
-- 40
--
-- >>> 0 & multiplying 4 +~ 12
-- 3.0
--
-- >>> 0 & adding 10 . multiplying 2 .~ 24
-- 2
--
-- >>> [1, 2, 3] & reversed %~ drop 1
-- [1, 2]
--
-- >>> (view flipped (++)) [1, 2] [3, 4]
-- [3,4,1,2]
--
-- >>> [1, 2, 3] ^. reversed
-- [3,2,1]

-- BONUS: Hard ones ahead!
-- >>> import Data.List (transpose)
--
-- Note: transpose flips the rows and columns of a nested list:
-- >>> transpose [[1, 2, 3], [10, 20, 30]]
-- [[1,10],[2,20],[3,30]]
-- >>> [[1, 2, 3], [10, 20, 30]] & involuted transpose %~ drop 1
-- [[2,3],[20,30]]

-- Extra hard: use `switchCase` somehow to make this statement work:
-- >>> import Data.Char (isUpper, toUpper, toLower)
-- >>> let switchCase c = if isUpper c then toLower c else toUpper c
-- >>> (32, "Hi") & _2 . involuted (fmap switchCase) .~ ("hELLO" :: String)
-- (32,"Hello")


-- 3.

fahrenheit :: Iso' Double Double
fahrenheit = iso celsiusToF fahrenheitToC
  where
    celsiusToF :: Double -> Double
    celsiusToF c = (c * (9/5)) + 32
    fahrenheitToC :: Double -> Double
    fahrenheitToC fah = (fah - 32) * (5/9)
-- >>> let temp = 35.0 -- Celsius
-- >>> temp ^. fahrenheit
-- 95.0
-- >>> temp ^. fahrenheit . from fahrenheit
-- 35.0



--------------------------------------------

-- Projecting Isos

-- mapping' :: Functor f => Iso' s a -> Iso' (f s) (f a)
-- mapping' i = iso (fmap (view i) (fmap (review i))

-- A more general version of this is provided in lens:

-- mapping :: (Functor f, Functor g)
--         => Iso s t a b -> Iso (f s) (g t) (f a) (g b)


-- Example:

toYamlList :: [String] -> String
toYamlList xs = "- " <> List.intercalate "\n- " xs

-- >>> let shoppingList = ["Milk", "Eggs", "Flour"] :: [Text]
-- >>> let strShoppingList = shoppingList ^. mapping unpacked :: [String]

-- 1)
-- >>> putStrLn $ toYamlList strShoppingList
-- - Milk
-- - Eggs
-- - Flour

-- 2)
-- >>> putStrLn $ shoppingList ^. mapping unpacked . to toYamlList

-- 3)
-- >>> traverseOf_ (mapping unpacked . to toYamlList) putStrLn shoppingList
-- >>> shoppingList & mapping unpacked . to toYamlList %%~ putStrLn
-- ^^^^^^^^ fails to compile


------------

-- What if we wanted to convert the function itself from [String] -> String to [Text] -> Text ?

--  * contramapping :: (Contravariant f) => Iso' s a -> Iso (f a) (f s)
--  * bimapping :: (Bifunctor f) => Iso' s a -> Iso' s' a' -> Iso' (f s s') (f a a')
--  * dimapping :: (Profunctor p) => Iso' s a -> Iso' s' a' -> Iso' (p a s') (p s a')


-- A function is an instance of profunctor, meaning we can contramap over its inputs an map over its output all in one go using dimapping!

textToYamlList :: [Text] -> Text
textToYamlList = toYamlList ^. dimapping (mapping unpacked)   packed
--                                        Iso' [Text] String  Iso String Text
--                                        ^^^ contramapped    ^^^ mapped

textToYamlList' :: [Text] -> Text
textToYamlList' = T.pack . toYamlList . fmap T.unpack
-- ^^^ Sometimes regular code is fine.


------------------------------

-- Exercises - Projected Isos

-- 1.
-- ("Beauty", "Age") ^. mapping reversed . swapped
-- ("egA", "Beauty")
--
-- >>> [True, False, True] ^. mapping (involuted not)
-- [False, True, False]
--
-- >>> [True, False, True] & mapping (involuted not) %~ filter id
-- [False]
--
-- >>> (show ^. mapping reversed) 1234
-- "4321"


-- 2.

-- enum :: Enum a -> Iso' Int a

intNot :: Int -> Int
intNot = not ^. dimapping enum (from enum)

-- >>> intNot 0
-- 1
-- >>> intNot 1
-- 0
-- >>> intNot 2
-- *** Exception

-- Simplify it

intNot' :: Int -> Int
intNot' = enum %~ not
-- ^^ smart



-----------------------

-- Isos and newtypes

-- Coercing with isos


-- coerced :: (Coercible s a, Coercible t b) => Iso s t a b


newtype Email = Email String
  deriving (Show)

newtype UserID = UserID String
  deriving (Show)

-- >>> over coerced
--       (reverse :: String -> String)
--       (Email "joe@example.com") :: Email


-- Type inference really struggles, solutions:

-- Define your helper:

--email :: Iso' Email String
--email = coerced

-- >>> Email "joe@example.com" & email . traversed %~ toUpper

-- Or take advantage of makeLenses which defines a Iso for newtypes:

newtype Email' = Email' {_email :: String}
  deriving (Show)
makeLenses ''Email'
-- ^^^^ email :: Iso' Email' String


----------------------

-- Newtype wrapper isos

-- Chris Penner recommends using the iso from makeLenses

-- _Wrapped' :: Wrapped s => Iso' s (Unwrapped s)
-- _Unwrapped' :: Wrapped s => Iso' (Unwrapped s) s

-- They have better type-inference because:
--  * Restricted forms of `coerced` which *only* map between newtypes and their unwrapped form (they won't map between Email and UserID).
-- * They don't allow type-changing transformations.


-- You need to create an instance!

-- makeWrapped ''Email
-- ^^^^ makes a _Wrapped', _Unwrapped' for  you

-- >>> Email "joe@example.com" & _Wrapped' . traversed %~ toUpper


-- This is an alternate version of _Wrapped' which let you pass the name of the Newtype constructor.

-- _Wrapping' :: Wrapped s => (Unwrapped s -> s) -> Iso'  s (Unwrapped s)

-- >>> Email "joe@example.com" & _Wrapping' Email . traversed %~ toUpper


--------------------------------

-- Laws

-- Isos are one of the cases where you really should make sure the laws hold.
-- Otherwise use a prism or a traversal instead.

-- 1. Reversability

-- myIso . from myIso == id
-- from myIso . myIso == id


-- >>> view (reversed . from reversed) ("Testing one two three")
-- "Testing one two three"
--
-- import Numeric.Lens
-- >>> view (negated . from negated) 23
-- 23

-- myIso :: Iso' Double Double
-- myIso = negated . adding 10.0 . multiplying 372.0
--
-- >>> view (myIso . from myIso) 23.0
-- 23.0
-- >>> view (from myIso . myIso) 23.0
-- 23.000000000000227


---------------------------------------

-- Exercises - Iso Laws


-- 1. Unlawful

mapList :: Ord k => Iso' (Map k v) [(k, v)]
mapList = iso Map.toList Map.fromList

-- >>> view (mapList . from mapList) $ Map.fromList [(1, 'A'), (2, 'B'), (2, 'C')]
-- fromList [(1,'A'),(2,'C')]


-- 2.

-- import Data.List.NonEmpty

nonEmptyList :: Iso [a] [b] (Maybe (NonEmpty a)) (Maybe (NonEmpty b))
nonEmptyList = iso to' from'
  where
    to' = NEL.nonEmpty
    from' = maybe [] NEL.toList

-- >>> view (nonEmptyList . from nonEmptyList) $ [1..10]
-- >>> view (nonEmptyList . from nonEmptyList) $ []
--
-- >>> view (from nonEmptyList . nonEmptyList) $ NEL.nonEmpty [1..10]
-- >>> view (from nonEmptyList . nonEmptyList) $ NEL.nonEmpty []

-- 3.

sorted :: Ord a => Iso' [a] [a]
sorted = iso List.sort undefined -- How do you get back the order ?
--- ^^^ Unlawful


-- 4.

sorted' :: Ord a => Iso' [a] [(Int, a)]
sorted' = iso to' from'
  where
    to' xs = List.sortOn snd $ zip [0..] xs
    from' xs = fmap snd $ List.sortOn fst xs

-- >>> view (sorted' . from sorted') $ [4,2,8,4,5,2]
-- >>> view (from sorted' . sorted') $ [(0, 4),(1, 2),(2, 8),(3, 4),(4, 5),(5, 2)]
-- ^^^^^^^^^^ fails to get this back
