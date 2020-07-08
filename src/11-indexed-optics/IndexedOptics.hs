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
{-# LANGUAGE DeriveFoldable      #-}
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
import           Data.Foldable

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
--   and only if it's an indexed optic.

-- The lens library provides several different index-aware composition operators:
--
--   * (<.)  use the index of the optic to the left.
--   * (.>)  use the index of the optic to the right (this is how (.) already behaves).
--   * (<.>) combine both indices as a tuple.

-- >>> agenda ^@.. itraversed <. itraversed
-- [("Monday","Shopping"),("Monday","Yoga"),("Saturday","Brunch"),("Saturday","Food coma")]
--
-- >>> agenda ^@.. itraversed <.> itraversed
-- [(("Monday",0),"Shopping"),(("Monday",1),"Yoga"),(("Saturday",0),"Brunch"),(("Saturday",1),"Food coma")]

-- Unlike normal (.), (<.>) is not associative:


------------------------------------

-- Custom Index Composition

-- icompose : compose any two indexed optics into a new indexed optic by combining the indexes together:

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


-- indices :: (Indexable i p, Applicative f)
--         => (i -> Bool) -> Optical' p (Indexed i) f a a

-- nb. This acts on the previous indexed optic not the accumulated one.
-- >>> exercises ^@.. (itraversed <. itraversed . indices (== "pushups"))
-- ^^^^^^^^ filters the indices from the second itraversed, not the first one !


-- >>> ['a'..'z'] ^.. itraversed . indices even
-- acegikmoqsuwy

-- >>> let ratings = Map.fromList [ ("Dark Knight", 94), ("Dark Knigth Rises", 87), ("Death of Superman", 92) ]
-- >>> import Data.List.Lens (prefixed)
-- >>> ratings ^.. itraversed . indices (has (prefixed "Dark"))
-- [94,87]



-- Target an *exact* index using:

-- index :: (Indexable i p, Eq i, Applicative f)
--       => i -> Optical' p (Indexed i) f a a



-- >>> ['a'..'z'] ^? itraversed . index 10
-- Just "k"
-- >>> ratings ^? itraversed . index "Death of Superman"
-- Just 92


-------------------------------

-- Exercises - Index Filters

exercises :: Map String (Map String Int)
exercises = Map.fromList
  [ ("Monday",    Map.fromList [ ("pushups", 10), ("crunches", 20) ])
  , ("Wednesday", Map.fromList [ ("pushups", 15), ("handstands", 3) ])
  , ("Friday",    Map.fromList [ ("crunches", 25), ("handstands", 5) ])
  ]

-- Compute the total number of "crunches" you should do this week
-- >>> sumOf (itraversed .> itraversed . indices (== "crunches")) exercises
-- 45

-- Compute the number of reps you need to do across all exercises types on Wednesday
-- >>> sumOf (itraversed . indices (== "Wednesday") . traversed) exercises
-- 18
-- This doesnt work: sumOf (itraversed <. itraversed . indices (== "Wednesday")) exercises

-- List out the number of pushups you need to do each day, you can use ix to help this time it you wish.

-- >>> exercises ^@.. (itraversed <. itraversed . indices (== "pushups"))
-- [("Monday", 10), ("Wednesday", 15)]


-- 2.

board :: [String]
board = [ "XOO"
        , ".XO"
        , "X.." ]

-- Generate a list of positions alongside their (row, column) coordinates.

showCoordinates :: Int -> Int -> String
showCoordinates x y = show (x, y)

-- >>> board ^@.. itraversed <.> itraversed
-- >>> board ^@.. icompose showCoordinates itraversed itraversed
-- [("(0,0)",'X'),("(0,1)",'O'),("(0,2)",'O'),("(1,0)",'.'),("(1,1)",'X'),("(1,2)",'O'),("(2,0)",'X'),("(2,1)",'.'),("(2,2)",'.')]

-- Set the empty suqare at (1,0) to an 'X'
--
-- >>> board & (itraversed <.> itraversed) . index (1,0) .~ 'X'
-- [ "XOO"
-- , "XXO"
-- , "X.." ]

-- Get the 2nd *column* as a list
--
-- >>> board ^.. (traverse . itraversed) . index 1
-- "OX."

-- Get the 3rd *row* as a list
-- >>> board ^.. (itraversed <. itraversed) . index 2
-- "X.."


--------------------------------------------

-- Custom indexed optics


-- Example: Tic-Tac-Toe

data Board a =
  Board
    a a a
    a a a
    a a a
  deriving (Show, Foldable)

-- We want to allowed indexing into the grind, but want to avoid using Ints because they might be out-of-bounds.

data Position = I | II | III
  deriving (Show, Eq, Ord)

testBoard :: Board Char
testBoard =
  Board
    'X' 'O' 'X'
    '.' 'X' 'O'
    '.' 'O' 'X'


-- Custom IndexedFolds

-- ifolding
-- ifolding
-- ifolding :: Foldable f => (s -> f(i, a)) -> IndexedFold i s a
-- ^^^ simplified

slotsFold :: IndexedFold (Position, Position) (Board a) a
slotsFold =
  ifolding $ \board ->
    zip [(x, y) | y <- [I, II, III], x <- [I, II, III]]
        (toList board)

-- >>> testBoard ^@.. slotsFold
-- [((I,I),'X'),((II,I),'O'),((III,I),'X'),((I,II),'.'),((II,II),'X'),((III,II),'O'),((I,III),'.'),((II,III),'O'),((III,III),'X')]


-- >>> testBoard ^@.. slotsFold . indices ((== II) . snd)
-- [((I,II),'.'),((II,II),'X'),((III,II),'O')]


-----------

-- Custom IndexedTraversals

-- What if we want to edit the slots in our board ? We need an IndexedTraversal!
--
-- Chris recommends implemented Ixed for Board instead.


-- There isn't a helper method for building custom traversals.

slotsTraversal :: IndexedTraversal (Position, Position) (Board a) (Board b) a b
slotsTraversal p (Board
                   a1 b1 c1
                   a2 b2 c2
                   a3 b3 c3)
  = Board <$> indexed p (I,   I) a1
          <*> indexed p (II,  I) b1
          <*> indexed p (III, I) c1
          <*> indexed p (I,   I) a2
          <*> indexed p (II,  I) b2
          <*> indexed p (III, I) c2
          <*> indexed p (I,   I) a3
          <*> indexed p (II,  I) b3
          <*> indexed p (III, I) c3
-- ^^^ Usually you write a normal Traversal and then just sprinkle indexed around wherever you call your handler.

-- >>> testBoard ^@.. slotsTraversal
-- [((I,I),'X'),((II,I),'O'),((III,I),'X'),((I,I),'.'),((II,I),'X'),((III,I),'O'),((I,I),'.'),((II,I),'O'),((III,I),'X')]


-- >>> testBoard & slotsTraversal . indices ((== II) . snd) .~ 'O'


printBoard :: Board Char -> IO ()
printBoard = itraverseOf_ slotsTraversal printSlot
  where
    printSlot (III, _) c = putStrLn [c]
    printSlot (_, _) c = putStr [c]

-- >>> printBoard testBoard
-- XOX
-- .XO
-- .OX


-------------------

-- Why we used indexed to implement our custom IndexedTraversal ?

-- Late take a look at the types of IndexedTraversal:
--
-- type IndexedTraversal i s t a b
--   = forall p f. (Indexable i p, Applicative f) => p a (f b) -> s -> t
--   ^^^^^ Indexable ~ Profunctor (like a handler but takes an index parameter)

-- indexed :: Indexable i p => p a b -> i -> a -> b
-- ^^^ any Indexable profunctor can be reduced into a regular function using indexed:


-- You can also write an indexed lens using the ilens helper:
--
-- ilens :: (s -> (i, a)) -> (s -> b -> t) -> IndexedLens i s t a b


------------------

-- Index helpers

-- Add indexes to optics:
--
-- indexing :: Traversal s t a b -> IndexedTraversal Int s t a b
-- indexing :: Lens s t a b -> IndexedLens Int s t a b
-- indexing :: Fold s a -> IndexedFold Int s a
-- indexing :: Getter s a -> IndexedGetter Int s a
-- ^^^^ adds a numeric index alongside its elements

-- >>> ("hello" :: Text) ^@.. indexing each
-- [(0,'h'),(1,'e'),(2,'l'),(3,'l'),(4,'o')]


-- We can re-map or edit the indexes of an optic using reindexed:

-- reindexed
-- reindexed
-- reindexed
-- reindexed :: Indexable j p => (i -> j) -> (Indexed i a b -> r) -> p a b -> r
-- ^^^^ map over the indexes of an optic

-- >>> ['a'..'c'] ^@.. reindexed (*10) itraversed
-- [(0,'a'),(10,'b'),(20,'c')]
--
-- >>> ['a'..'c'] ^@.. reindexed show itraversed
-- [("0",'a'),("1",'b'),("2",'c')]

-- import Data.Map.Lens
-- >>> toMapOf (reindexed show itraversed) ['a'..'c']


------

-- Another helper is:

-- selfIndex
-- selfIndex :: Indexable a p => p a fb -> a -> fb
-- selfIndex
-- ^^^^ `selfIndex` copies a snapshot of the current focus into the index

-- >>> [("Betty", 37), ("Veronica", 12)] ^@.. itraversed . selfIndex <. _2
-- [(("Betty",37),37),(("Veronica",12),12)]
-- >>> [1..5] ^@.. selfIndex <. itraversed
-- [([1,2,3,4,5],1),([1,2,3,4,5],2),([1,2,3,4,5],3),([1,2,3,4,5],4),([1,2,3,4,5],5)]


---------------------------

-- Exercises - Custom Indexed Optics

-- 1.

pair :: IndexedFold Bool (a, a) a
pair = ifolding $ \(a, b) -> [(False, a), (True, b)]
-- >>> ('a', 'b') ^@.. pair
-- [(False,'a'),(True,'b')]

pair' :: IndexedTraversal Bool (a, a) (b, b) a b
pair' p (a, a1) = liftA2 (,) (indexed p False a) (indexed p True a1)
-- >>> ('a', 'b') & pair' %@~ (\p a -> if p then [a,a] else [])
-- ("","bb")

-- 2. Use reindexed to provide an indexed list traversal which starts at '1' instead of '0'

oneIndexed :: IndexedTraversal Int [a] [b] a b
oneIndexed = reindexed (+1) (indexing traversed)

-- >>> ['a'..'d'] ^@.. oneIndexed
-- [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd')]

invertedIndex :: IndexedTraversal Int [a] [b] a b
invertedIndex =
  reindexed
    (\(xs, i) -> (length xs - 1) - i)
    (selfIndex <.> itraversed)
-- >>> ['a'..'d'] ^@.. invertedIndex
-- [(3, 'a'), (2, 'b'), (1, 'c'), (0, 'd')]


-- 3.

chars :: IndexedTraversal Int Text Text Char Char
chars = indexing each
-- >>> ("banana" :: Text) ^@.. chars

charCoords :: IndexedTraversal (Int, Int) String String Char Char
charCoords = indexing lined <.> itraversed
-- >>> "line\nby\nline" ^@.. charCoords
-- [ ((0,0),'l'),((0,1),'i'),((0,2),'n'),((0,3),'e')
-- , ((1,0),'b'),((1,1),'y')
-- , ((2,0),'l'),((2,1),'i'),((2,2),'n'),((2,3),'e')
-- ]


---------------------------------------------

-- Index-preserving optics


-- Carrying the index around with (<.) and (<.>) is annoying.
--
-- The lens library provides a way around that in the way of **index-preserving optics**


-- This won't compie because (_1) forgets about the index from itraversed and (^@..) requires an indexed optics.
-- >>> [('a', True), ('b', False), ('c', True)] ^@.. itraversed . _1
--
-- We can fix it by using an index composition operator:
-- >>> [('a', True), ('b', False), ('c', True)] ^@.. itraversed <. _1

-- We can also use an index-preserving optic:
-- >>> let _1' = cloneIndexPreservingLens _1
-- >>> [('a', True), ('b', False), ('c', True)] ^@.. itraversed <. _1'



-- cloneIndexPreservingLens :: Lens s t a b
--                          -> IndexPreservingLens s t a b
--
-- cloneIndexPreservingTraversal :: Traversal s t a b
--                               -> IndexPreservingTraversal s t a b
--
-- cloneIndexPreservingSetter :: Setter s t a b
--                            -> IndexPreservingSetter s t a b


-- Strangely, cloneIndexPreservingFold seems to be missing from the library

-- There's also an iplens combinator for constructing index-preserving lenses from scratch.
-- Lenses are the most likely to be IndexPreserving since folds and traversals are more likely to have their own indexess

-- iplens :: (s -> a) -> (s -> b -> t) -> IndexPreservingLens s t a b
