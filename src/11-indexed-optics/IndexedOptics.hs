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
module IndexedOptics where

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

-- Indexed Optics

-- accumulate information about your current focus

-- Traversing a list provide the list-index
-- Traversing a map provide the key-index
-- Traversing a tree provide the location on the tree.



-- All combinators work seamlessly with indices !
-- *Indexed optics compose just fine with indexed and non-indexed optics alike*

-- It's *very important* to note that it's the *action* which adds the index to the result; the
-- index isn't part of the *focus*.

-- itraversed :: TraversableWithIndex i t => IndexedTraversal i (t a) (t b) a b
-- ^^^^ We can use it in places of traversed
--
-- >>> toListOf itraversed ["Summer", "Fall", "Winter", "Spring"]
-- ["Summer","Fall","Winter","Spring"]

-- itoListOf, ^@..
-- itoListOf :: IndexedFold i s a -> s -> [(i, a)]

-- >>> itoListOf itraversed ["Summer", "Fall", "Winter", "Spring"]
-- [(0,"Summer"),(1,"Fall"),(2,"Winter"),(3,"Spring")]
-- >>> ["Summer", "Fall", "Winter", "Spring"] ^@.. itraversed

-- toListOf   (^..)     itoListOf    (^@..)
-- over       (%~)      iover        (%@~)
-- traverseOf (%%~)     itraverseOf  (%%@~)
-- set        (.~)      iset         (.@~)
-- view       (^.)      iview        (^@.)


-- >>> [10, 20, 30] & itraversed %@~ (+)
-- [10, 21, 32]


-- There are indexed versions of Lens, Fold, Getter, Setter, etc

-- Typically you don't need an index when accessing only a single element with a lens, prism or iso.
-- Folds and traversals are the most common.

-- Index of map = key
-- >>> let agenda = Map.fromList [("Monday", "Shopping"), ("Tuesday", "Swimming")]
-- >>> agenda ^@.. itraversed
-- [("Monday","Shopping"),("Tuesday","Swimming")]
--
-- Index of tuple = first half
-- >>> (True, "value") ^@.. itraversed
-- (True, "value")

-- >>> import Data.Tree
-- >>> let t = Node "top" [Node "left" [], Node "right" []]
-- >>> t ^@.. itraversed
-- [([],"top"),([0],"left"),([1],"right")]


---------------------------------------

-- Index Composition


-- >>> let agenda = Map.fromList [ ("Monday", ["Shopping", "Yoga"]),   ("Saturday", ["Brunch", "Food coma"])]
-- >>> agenda ^@.. itraversed . itraversed
-- [(0,"Shopping"),(1,"Yoga"),(0,"Brunch"),(1,"Food coma")]
-- ^^^^ we lost track of the day of the week ..

-- When composing optics the index of the whole path is the index of the *last* optic,
-- and only if it's an indexed optic.

-- The lens library provides several different index-aware composition operators:
--   * (<.)  use the index of the optic to the left
--   * (.>)  use the index of the optic to the right (this is how (.) already behaves)
--   * (<.>) cmbine both indices as a tuple

-- >>> agenda ^@.. itraversed <. itraversed
-- [("Monday","Shopping"),("Monday","Yoga"),("Saturday","Brunch"),("Saturday","Food coma")]
--
-- >>> agenda ^@.. itraversed <.> itraversed
-- [(("Monday",0),"Shopping"),(("Monday",1),"Yoga"),(("Saturday",0),"Brunch"),(("Saturday",1),"Food coma")]

-- Unlike normal (.), (<.>) is not associative:


------------------------------------

-- Custom Index Composition

-- icompose : compose any two ndexed optics into a new indexed optic by combining the indexes together:

-- icompose :: (i -> j -> k)
--          -> IndexedOptics i s t a b
--          -> IndexedOptics j a b c d
--          -> IndexedOptics k s t c d
-- ^^^^ The real signature is a mess.


showDayAndNumber :: String -> Int -> String
showDayAndNumber = (\a b -> a <> ": " <> show b)

-- >>> agenda ^@.. icompose showDayAndNumber itraversed itraversed
-- [ ("Monday: 0","Shopping")
-- , ("Monday: 1","Yoga")
-- , ("Saturday: 0","Brunch")
-- , ("Saturday: 1","Food coma")
-- ]

-- If you use it a lot, it's better to define an operator

(.++) :: (Indexed String s t -> r)
      -> (Indexed String a b -> s -> t)
      -> Indexed String a b -> r
(.++) = icompose (\a b -> a ++ ", " ++ b)

populationMap :: Map String (Map String Int)
populationMap = Map.fromList
  [ ( "Canada", Map.fromList [ ("Ottawa", 999234), ("Toronto", 2313131) ] )
  , ( "Germany", Map.fromList [ ("Berlin", 3232131), ("Munich", 123413241) ] )
  ]
-- >>> populationMap ^@.. itraversed .++ itraversed
-- [("Canada, Ottawa",999234)
-- ,("Canada, Toronto",2313131)
-- ,("Germany, Berlin",3232131)
-- ,("Germany, Munich",123413241)
-- ]

-- When defining a new composition operator like this you can follow this template:

-- (<symbol>) :: (Indexed <indexTypeA> s t -> r)
--            -> (Indexed <indexTypeB> a b -> s -> t)
--            -> Indexed <combinedType> a b -> r
-- (<symbol>) = icompose <combinationFunction>

-- Or you can leave it without type signature and GHCi will infere it.

------------------------------------------------

-- Exercises - Indexed Optics

-- 1.

-- >>> Map.fromList [("streamResponse", False), ("useSSL", True)]
--       ^@.. itraversed
-- [("streamResponse",False),("useSSL",True)]

-- >>> (Map.fromList [('a', 1), ('b', 2)], Map.fromList [('c', 3), ('d', 4)])
--        ^@.. both . itraversed
-- [('a',1),('b',2),('c',3),('d',4)]

-- >>> Map.fromList [('a', (True, 1)), ('b', (False, 2))]
--       ^@.. itraversed <. _1
-- [('a', True), ('b', False)]

-- >>> [ Map.fromList [("Tulips", 5), ("Roses", 3)]
--     , Map.fromList [("Goldfish", 11), ("Frogs", 8)]
--     ] ^@.. itraversed <.> itraversed
-- [ ((0,"Roses"), 3)
-- , ((0,"Tulips"), 5)
-- , ((1,"Frogs"), 8)
-- , ((1,"Goldfish"), 11)
-- ]

-- >>> [10, 20, 30] & itraversed %@~ (+)
-- [10, 21, 32]

-- >>> itraverseOf_
--       itraversed
--       (\i s -> putStrLn (replicate i ' ' <> s))
--       ["one", "two", "three"]
-- one
--   two
--     three

-- >>> itraverseOf_
--       itraversed
--       (\n s -> putStrLn $ show n ++ ": " ++ s)
--       ["Go shopping", "Eat lunch", "Take a nap"]
-- 0: Go shopping
-- 1: Eat lunch
-- 2: Take a nap


------------------------------------------------------

-- Filtering by index



















