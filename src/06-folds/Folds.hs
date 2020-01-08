{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Folds where

-------------------------------------

import           Control.Applicative
import           Control.Lens        as L
import           Control.Monad.State
import           Data.Char
import           Data.Function       (on)
import qualified Data.Map            as Map
import           Data.Monoid
import           Data.Ord            (comparing)
import qualified Data.Set            as Set
import qualified Data.Text           as T

-------------------------------------

-- FOLDS:
-- * Lenses must focus ONE thing, Folds can focus MANY things.
-- * Lenses can get and set, Folds can **only get**

-- Folds don't have laws.

-- Query language for asking complex questions about our structures.

-- Folds focus an arbitrary number of elements and combine them all according to some recipe.

data Role
  = Gunner
  | PowderMonkey
  | Navigator
  | Captain
  | FirstMate
  deriving (Show, Eq, Ord)

data CrewMember =
  CrewMember { _name    :: String
             , _role    :: Role
             , _talents :: [String]
             }
    deriving (Show, Eq, Ord)
makeLenses ''CrewMember

roster :: Set.Set CrewMember
roster = Set.fromList
  [ CrewMember "Grumpy Roger" Gunner ["Juggling", "Arbitrage"]
  , CrewMember "Long-John Bronze" PowderMonkey ["Origami"]
  , CrewMember "Salty Steve" PowderMonkey ["Charcuterie"]
  , CrewMember "One-eyed Jack" Navigator []
  ]

-- Fold s a        since folds can't be used for setting, there aren't any polymorphic actions which run on folds.
--
-- This type says that if you give us a structure of type s we can find zero or more focuses of type a.
-- Note that a fold only specifies how to find the focuses, not how to combine them.

-- We need something like this:
--   rosterRoles :: Fold (Set.Set CrewMember) Role
--
--
-- 1. crewMembers :: Fold (Set.Set CrewMember) CrewMember
-- 2. crewRole :: Fold CrewMember Role
-- 3. We'll need an action something lis this:
--      toListSomehow :: Fold (Set.Set CrewMember) Role -> Set.Set CrewMember -> [Role]



-- 1. folded :: Foldable f => Fold (f a) a
--    folded takes ANY Foldable container as a struture and will focus each element inside it

--crewMembers :: Fold (Set.Set CrewMember) CrewMember
--crewMembers = folded

-- action for Fold:
-- toListOf (^..): retrieves zero or more in a list.
--
-- toListOf :: Fold s a -> s -> [a]
-- (^..)    :: Fold s a -> s -> [a]

-- >>> toListOf folded roster
-- >>> roster ^.. folded

-- >>> Just "Buried Treasure" ^.. folded
-- ["Buried Treasure"]
-- >>> Nothing ^.. folded
-- []
-- >>> Identity "Atlas" ^.. folded
-- ["Atlas"]
-- >>> ("Rubies", "Gold") ^.. folded
-- ["Gold"]
-- >>> Map.fromList [("Jack", "Captain"), ("Will", "First Mate")] ^.. folded
-- ["Captain","First Mate"]

----------------------

-- Using lenses as folds: you can use a lens anywhere you need a fold.

--crewRole :: Fold CrewMember Role
--crewRole = role

-- >>> let jerry = CrewMember "Jerry" PowderMonkey ["Ice Cream Making"]
-- >>> jerry ^.. role
-- [PowderMonkey]

------------------

-- Composing folds

-- >>> roster ^.. folded . role
-- [Gunner,PowderMonkey,Navigator,PowderMonkey]

-- folded :: Fold (CrewMember) CrewMember
-- role   :: Fold CrewMember Role

------------------

-- Foundational fold combinators

-- both :: Bitraversable r => Traversal (r a a) (r b b) a b
--
-- Every traversal is also a valid fold.
--
-- both :: Bitraversable r => Fold (r a a) a

-- Bitraversable are Traversal over two type parameters like: (a, b), Either e a
-- fold allows us to fold over both parameters when the parameters are the same.

-- >>> ("Gemini", "Leo") ^.. both
-- ["Gemini", "Leo"]
-- >>> Left "Albuquerque" ^.. both
-- ["Albuquerque"]
-- >>> Right "Yosemite" ^.. both
-- ["Yosemite"]
-- >>> ("Gemini", "Leo", "Libra") ^.. both
-- ["Gemini", "Leo"]


-- each :: Each s t a b => Traversal s t a b
--    Simplified
-- each :: Each s s a a => Fold s a

-- >>> (1, 2, 3, 4, 5) ^.. each
-- [1,2,3,4,5]
-- >>> [1, 2, 3, 4, 5] ^.. each
-- [1,2,3,4,5]
-- >>> ("Made him an offer" :: T.Text) ^.. each
-- "Made him an offer" :: String
-- >>> ("Do or do not" :: BS.ByteString) ^.. each
-- [68,111,32,111,114,32,100,111,32,110,111,116] :: GHC.Word8

-- Exercises - Simple Folds

-- 1.

beastSizes :: [(Int, String)]
beastSizes = [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]

-- >>> beastSizes ^.. folded
-- [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]
-- >>> beastSizes ^.. folded.folded
-- ["Sirens","Kraken","Ogopogo"]
-- >>> beastSizes ^.. folded.folded.folded
-- "SirensKrakenOgopogo"
-- >>> beastSizes ^.. folded . _2
-- ["Sirens","Kraken","Ogopogo"]
-- >>> toListOf (folded . folded) [[1,2,3], [4,5,6]]
-- [1,2,3,4,5,6]
-- >>> toListOf (folded . folded) (Map.fromList [("Jack", "Captain"::String), ("Will", "First Mate")])
-- "CaptainFirst Mate"
-- >>> ("Hello", "It's me"::String) ^.. both . folded
-- "HelloIt's me"
-- >>> ("Why", "So", "Serious?") ^.. each
-- ["Why", "So", "Serious?"]

quotes :: [(T.Text, T.Text, T.Text)]
quotes = [("Why", "So", "Serious?"), ("This", "is", "SPARTA")]

-- >>> quotes ^.. each . each . each
-- "WhySoSerious?ThisisSPARTA"

-- 2.

-- folded :: Fold [(Int, Char)] (Int, Char)
-- _1 :: Fold (Int, Char) Int

-- _2 :: Fold (Bool, Set String) (Set String)
-- folded :: (Set String) String
-- toListOf :: Foldable s => Fold (Bool, Set String) String -> (Bool, Set String) -> [String]

-- 3.

-- folded
-- _1
-- folded.both
-- folded._1
-- folded._2.folded
-- each

-----------------------

-- Custom Folds


newtype Name = Name { getName :: String }
  deriving Show

data ShipCrew = ShipCrew
  { _shipName   :: Name
  , _captain    :: Name
  , _firstMate  :: Name
  , _conscripts :: [Name]
  } deriving (Show)

makeLenses ''ShipCrew

-- folding :: Foldable f => (s -> f a) -> Fold s a

collectCrewMembers :: ShipCrew -> [Name]
collectCrewMembers crew =
  [_captain crew, _firstMate crew] ++ _conscripts crew

crewMembers :: Fold ShipCrew Name
crewMembers = folding collectCrewMembers

myCrew :: ShipCrew
myCrew = ShipCrew
  { _shipName = Name "Purple Pearl"
  , _captain = Name "Grumpy Roger"
  , _firstMate = Name "Long-John Bronze"
  , _conscripts = [Name "One-eyed Jack", Name "Filthy Frank"]
  }

-- >>> myCrew ^.. crewMembers
-- [Name {getName = "Grumpy Roger"},Name {getName = "Long-John Bronze"},Name {getName = "One-eyed Jack"},Name {getName = "Filthy Frank"}]

-----------------------

-- Mapping over folds

-- to :: (s -> a) -> Fold s a

-- >>> Name "Two-face Tony" ^. to getName
-- "Two-face Tony"

-- >>> Name "Two-face Tony" ^. to getName . to (fmap toUpper)
-- "TWO-FACE TONY" :: [Char]

-- >>> myCrew ^.. crewMembers . to getName
-- ["Grumpy Roger","Long-John Bronze","One-eyed Jack","Filthy Frank"]


-----------------------

-- Combining multiple folds on the same structure

-- We can use each of the smaller folds inside a call to folding to build up a bigger fold:

-- Every Lens is a valid fold!
crewNames :: Fold ShipCrew Name
crewNames =
  folding (\s -> s ^.. captain
              <> s ^.. firstMate
              <> s ^.. conscripts.folded)

-- >>> myCrew ^.. crewNames . to getName
-- ["Grumpy Roger","Long-John Bronze","One-eyed Jack","Filthy Frank"]


-- Exercises - Custom Folds

-- 1.
-- >>> [[1,2,3],[4,5,6]] ^.. folded . folding (take 2)
-- [1,2,4,5]
-- >>> [[1,2,3],[4,5,6]] ^.. folded . to (take 2)
-- [[1,2],[4,5]]
-- >>> [[1,2,3],[4,5,6]] ^.. folded . to (take 2) . folded
-- [1,2,4,5]
-- >>> ["bob", "otto", "hannah"] ^.. folded . to reverse
-- ["bob", "otto", "hannah"]
-- ("abc", "def") ^.. folding (\(a, b) -> [a, b]) . to reverse . folded
-- "cbafed"

-- 2.
-- >>> [1..5] ^.. folded . to (*100)
-- [100,200,300,400,500]
-- >>> (1, 2) ^.. each
-- [1, 2]
-- [(1, "one"), (2, "two")] ^.. folded . _2
-- ["one", "two"]
-- >>> (Just 1, Just 2, Just 3) ^.. each . folded
-- [1,2,3]
-- >>> [Left 1, Right 2, Left 3] ^.. folded . folded
-- [2]
-- >>> [([1, 2], [3, 4]), ([5, 6], [7, 8])] ^.. folded . each . folded
-- [1, 2, 3, 4, 5, 6, 7, 8]
-- >>> [1, 2, 3, 4] ^.. folded . to (\n -> if odd n then Left n else Right n)
-- [Left 1, Right 2, Left 3, Right 4]
-- >>> [(1, (2, 3)), (4, (5, 6))] ^.. folded . folding (\(a, (b, c)) -> [a,b,c])
-- [1, 2, 3, 4, 5, 6]
-- >>> [(Just 1, Left "one"), (Nothing, Right 2)] ^..
--        folded . folding (\(a, b) -> a ^.. folded <> b ^.. folded)
-- [1, 2]
-- >>> [(1, "one"), (2, "two")] ^.. folded . folding (\(a, b) -> [Left a, Right b])
-- [Left 1, Right "one", Left 2, Right "two"]
-- >>> Set.fromList ["apricots", "apples"] ^.. folded . folding reverse
-- "selppastocirpa"

-- 3.
-- >>> [(12, 45, 66), (91, 123, 87)] ^.. _
-- [(12, 45, 66), (91, 123, 87)]
--        ^.. folded
--        . _2
--        . to show
--        . to reverse
--        . folded
-- "54321"
-- [(1, "a"), (2, "b"), (3, "c"), (4, "d")]
--    ^.. folded
--    . folding (\(n, a) -> if even n then [a] else [])
-- ["b", "d"]

-----------------------

-- Fold Actions

-- Query data from your structure

-- A rule of thumb when lookign for which action to use on a fold: think of the function
-- you'd use on normal ol' Haskell list for the same purpose, then just add the suffix -Of!
-- sumOf, minimumOf, ...

-- Most of these operations are optimized and the actual types are strange:
-- sumOf :: Num a => Getting (Endo (Endo a)) s a -> s -> a

-- When I see a Getting (Some Crazy Type) s a I know I can substittue nearly any optic into
-- that slot including a (Fold s a) or a (Lens' s a).

-- There are A LOT of these actions, and they're mostly named in a straightforward way.

-- > The list of the examples can be substituted by any foldable structure.

-- elemOf :: Eq a => Fold s a -> a -> s -> Bool
-- >>> elemOf folded 3 [1, 2, 3, 4]
-- True

-- anyOf :: Fold s a -> (a -> Bool) -> s -> Bool
-- >>> anyOf folded even [1, 2, 3, 4]
-- True

-- allOf :: Fold s a -> (a -> Bool) -> s -> Bool
-- >>> allOf folded (<10) [1, 2, 3, 4]
-- True

-- findOf :: Fold s a -> (a -> Bool) -> s -> Maybe a
-- >>> findOf folded even [1,2,3,4]
-- Just 2

-- has    :: Fold s a -> s -> Bool
-- hasn't :: Fold s a -> s -> Bool
-- >>> has folded []
-- False
-- >>> hasn't folded []
-- True

-- lengthOf :: Fold s a -> s -> Int
-- >>> lengthOf folded [1,2,3,4]
-- 4

-- sumOf     :: Num n => Fold s n -> s -> n
-- productOf :: Num n => Fold s n -> s -> n
-- >>> sumOf folded [1..10]
-- 55
-- >>> productOf folded [1..10]
-- 3628800

-- firstOf, preview and (^?) are the same
--
-- firstOf :: Fold s a -> s -> Maybe a
-- preview :: Fold s a -> s -> Maybe a
-- (^?)    :: s -> Fold s a -> Maybe a
-- lastOf  :: Fold s a -> s -> Maybe a
-- >>> firstOf folded []
-- Nothing
-- >>> firstOf folded [1,2,3,4]
-- >>> preview folded [1,2,3,4]
-- >>> [1,2,3,4] ^? folded
-- Just 1
-- >>> lastOf folded [1..10]
-- 10

-- minimumOf :: Ord a => Fold s a -> s -> Maybe a
-- maximumOf :: Ord a => Fold s a -> s -> Maybe a
-- >>> minimumOf folded [2,1,4,3]
-- 1
-- >>> maximumOf folded [2,1,4,3]
-- 4

-- maximumByOf :: Fold s a -> (a -> a -> Ordering) -> s -> Maybe a

data Actor =
  Actor { _actorName :: String
        , _birthYear :: Int
        } deriving (Show, Eq)
makeLenses ''Actor

data TVShow =
  TVShow { _title       :: String
         , _numEpisodes :: Int
         , _numSeasons  :: Int
         , _criticScore :: Double
         , _actors      :: [Actor]
         } deriving (Show, Eq)
makeLenses ''TVShow

howIMetYourMother :: TVShow
howIMetYourMother = TVShow
  { _title = "How I Met Your Mother"
  , _numEpisodes = 208
  , _numSeasons = 9
  , _criticScore = 83
  , _actors =
    [ Actor "Josh Radnor" 1974
    , Actor "Cobie Smulders" 1982
    , Actor "Neil Patrick Harris" 1973
    , Actor "Alyson Hannigan" 1974
    , Actor "Jason Segel" 1980
    ]
  }

buffy :: TVShow
buffy = TVShow
  { _title = "Buffy the Vampire Slayer"
  , _numEpisodes = 144
  , _numSeasons = 7
  , _criticScore = 81
  , _actors =
    [ Actor "Sarah Michelle Gellar" 1977
    , Actor "Alyson Hannigan" 1974
    , Actor "Nicholas Brendon" 1971
    , Actor "David Boreanaz" 1969
    , Actor "Anthony Head" 1954
    ]
  }

tvShows :: [TVShow]
tvShows = [ howIMetYourMother
          , buffy
          ]

-- >>> sumOf (folded . numEpisodes) tvShows
-- 352

-- >>> maximumOf (folded . criticScore) tvShows
-- Just 83.0

-- >>> import Data.Ord (comparing)
-- >>> _title <$> maximumByOf folded (comparing _criticScore) tvShows
-- Just "How I Met Your Mother"
-- >>> minimumByOf (folded . actors . folded) (comparing _birthYear) tvShows
-- Just (Actor {_actorName = "Anthony Head", _birthYear = 1954})

-- Let's write a combinator for comparing instead of using all the time `comparing _...`

comparingOf :: Ord a => Lens' s a -> s -> s -> Ordering
comparingOf l = comparing (view l)

-- >>> minimumByOf (folded . actors . folded) (comparingOf birthYear) tvShows
-- Just (Actor {_actorName = "Anthony Head", _birthYear = 1954})

--------------

-- Folding with effects

-- traverseOf_ :: Functor => Fold s a -> (a -> f r) -> s -> f ()
-- forOf_ :: Functor f => Fold s a -> s -> (a -> f r) -> f ()

calcAge :: Actor -> Int
calcAge actor = 2030 - _birthYear actor

showActor :: Actor -> String
showActor actor = _actorName actor <> ": " <> show (calcAge actor)

printActors =
  traverseOf_ ( folded
              . actors
              . folded
              . to showActor
              )
              putStrLn
              tvShows

-- >>> execState (traverseOf_ folded (modify . const (+1)) tvShows) 0
-- 2

------------------

-- Combining fold results

-- All of the fold actions we've been using so far (e.g. maximumOf, lengthOf, anyOf, findOf, etc..) are all iplemented using Monoids.

-- foldOf    :: Monoid a => Fold s a -> s -> a
-- foldMapOf :: Monoid r => Fold s a -> (a -> r) -> s -> r
--
-- You'll likely only need  these actions when working with your own Monoids.

-- The real signatures:
--
-- foldOf    :: Getting a s a -> s -> a
-- foldMapOf :: Getting r s a -> (a-> r) -> s -> r


-- Example: compute the average age of all the actors

ageSummary :: Actor -> (Sum Int, Sum Int)
ageSummary actor = (Sum 1, Sum (calcAge actor))

computeAverage :: (Sum Int, Sum Int) -> Double
computeAverage (Sum count, Sum total) = fromIntegral total / fromIntegral count

-- >>> computeAverage $ foldOf (folded . actors . folded . to ageSummary) tvShows
-- 57.2

-- foldMapOf exists specifically for this sort of case where we need to transform our data
-- into a specific Monoid before we fold it.

-- >>> computeAverage $ foldMapOf (folded . actors . folded) ageSummary tvShows
-- 57.2

---------------

-- Using `view` on folds

-- > This behaviour is very confusing. Use `foldOf` explicitly when you want this behaviour.

-- View (^.) on a fold works in some cases:

-- This one works just fine
-- >>> Just "do it" ^. folded
-- "do it"
--
-- This one crashes and burns!
-- >>> Just (42 :: Int) ^. folded
-- error:
--  - No instance for (Monoid Int) arising from a use of 'folded' ....

-- Why ? The implementation of view accepts any type of optic, but adds constraints which usually only work on a lens. HOWEVER, if the focus you’re viewing happens to be a **Monoid** it can “view” through the foldby using the Monoid typeclass.
--
-- If the focus is a Monoid, then view can return mempty (1) or can use (<>) to combine all the focuses into a single focus (2).

-- (1)
-- >>> Nothing ^. folded :: String
-- ""
--
-- (2)
-- >>> ("one", "two", "three") ^. each
-- "onetwothree"

-----------------------

-- Customizing monoidal folds

-- Number of times an actor's name occurs amongst all our focuses. We can keep the summary of the occurrences as a Map String Int.
--
-- >>> foldMapOf (folded . actors . folded . actorName) (\name -> Map.singleton name 1) tvShows
-- fromList [("Alyson Hannigan",1),("Anthony Head",1),("Cobie Smulders",1),("David Boreanaz",1),("Jason Segel",1),("Josh Radnor",1),("Neil Patrick Harris",1),("Nicholas Brendon",1),("Sarah Michelle Gellar",1)]

-- > When we combine two maps with the same keys it simply ignores duplicate values of the same key rathen than accumulating them:
--
-- >>> Map.singleton 'a' "first" <> Map.singleton 'a' "second"
-- fromList [('a', "first")]

-- foldByOf    :: Fold s a -> (a -> a -> a) -> a -> s -> a
-- foldMapByOf :: Fold s a -> (r -> r -> r) -> r -> (a -> r) -> s -> r
-- foldrOf     :: Fold s a -> (a -> r -> r) -> r -> s -> r
-- foldlOf     :: Fold s a -> (r -> a -> r) -> r -> s -> r

-- foldByOf/foldMapByOf are like foldOf/foldMapOf but let us define the Monoidal identity and combining action.


-- >>> foldMapByOf
--       (folded . actors . folded . actorName)
--       (Map.unionWith (+))
--       mempty
--       (\name -> Map.singleton name 1)
--       tvShows
--
-- fromList
--   [ ("Alyson Hannigan",2)
--   , ("Anthony Head",1)
--   ...
--   ]


--------------------------

-- Exercises - Fold Actions

-- 1. Pick the matching action from the list for each example:

-- >>> has folded []
-- False
-- >>> foldOf both ("Yo", "Adrian!")
-- "YoAdrian!"
-- >>> elemOf each "phone" ("E.T.", "phone", "home")
-- True
-- >>> minimumOf folded [5, 7, 2, 3, 13, 17, 11]
-- Just 2
-- >>> lastOf folded [5, 7, 2, 3, 13, 17, 11]
-- Just 11
-- >>> anyOf folded ((> 9) . length) ["Bulbasaur", "Charmander", "Squirtle"]
-- True
-- >>> findOf folded even [11, 22, 3, 5, 6]
-- Just 22


-- 2.

-- Find the first word in the input list which is a palindrome; I.e. a word that’s the same backwards as forwards.

-- >>> findOf folded (\s -> s == reverse s) ["umbrella", "olives", "racecar", "hammer"]
-- Just "racecar"

-- Determine whether all elements of the following tuple are EVEN
-- >>> allOf each even (2, 4, 6)
-- True

-- Find the pair with the largest integer (nb. comparingOf was made by us)
-- >>> maximumByOf folded (comparingOf _1) [(2, "I'll"), (3, "Be"), (1, "Back")]
-- Just (3,"Be")

-- Find the sum of both elements of a tuple. This one may require additional alterations AFTER running
-- >>> sumOf both (1, 2)   -- Alternative: getSum $ foldMapOf both Sum (1, 2)
-- 3


-- 3.
-- Find which word in a string has the most vowels.
-- >>> maximumByOf
--       worded
--       (compare `on` (length . filter (`elem` "aeiou")))
--       "Do or do not, there is no try."
-- Just "there"
--
--   worded == folding words
--   ((+) `on` f) x y = f x + f y

-- Combine the elements into the expected String
-- >>> foldOf (to reverse . folded) ["a", "b", "c"]
-- >>> foldByOf folded (flip (++)) mempty ["a", "b", "c"]
-- >>> foldrOf folded (flip (++)) mempty ["a", "b", "c"]
-- "cba"

-- Good luck with the following ones! Get it to work however you can!
--
-- >>> foldOf (folded . _2 . to (reverse . show)) [(12, 45, 66), (91, 123, 87)]
-- "54321"

-- >>> [(1, "a"), (2, "b"), (3, "c"), (4, "d")]
--       ^.. folded
--       . folding (\(a, b) ->
--                   if even a
--                     then [b]
--                     else []
--                 )
-- ["b", "d"]

--------------------------

-- Higher Order Folds


-- Set of optics combinators which **alter other optics** (higher-order combinators).
-- They accept an optic as an argument and return a new one as a result.

-- taking and dropping: these are technically **traversals** (all  traversals are valid folds)

-- taking
--   :: (Conjoined p, Applicative f)
--   => Int
--   -> Traversing p f s t a a
--   -> Over p f s t a a

-- dropping
--   :: (Conjoined p, Applicative f)
--   => Int
--   -> Over p (Control.Lens.Internal.Indexed.Indexing f) s t a a
--   -> Over p f s t a a

-- If you look at the documentation we'll see a whole list of valid specialized signatures:

-- taking   :: Int -> Fold s a -> Fold s a
-- dropping :: Int -> Fold s a -> Fold s a

-- They operate on the Fold rather than on the structure.

-- >>> [1..5] ^.. taking 2 folded
-- [1,2]
-- >>> [1..5] ^.. dropping 2 folded
-- [3,4,5]

-- Be careful with folding branches:
--
-- [[1,2,3], [10,20,30], [100,200,300]] ^.. folded . taking 2 folded
-- [1,2,10,20,100,200]
--
-- [[1,2,3], [10,20,30], [100,200,300]] ^.. taking 2 (folded . folded)
-- [1,2]

---------------

-- Another high-order fold:
--   backwards :: Fold s a -> Fold s a
--
-- *reality*
--   backwards :: (Profunctor p, Profunctor q)
--             => Optical p q (Backwards f) s t a b
--             -> Optical pq f s t a b

-- >>> [1,2,3] ^.. backwards folded
-- [3,2,1]
-- >>> ("one", "two") ^.. backwards both
-- ["two","one"]
-- >>> [(1,2),(3,4)] ^.. backwards (folded . both)
-- [4,3,2,1]
-- >>> [(1,2),(3,4)] ^.. backwards folded . both
-- [3,4,1,2]
-- >>> [(1,2),(3,4)] ^.. folded . backwards both
-- [2,1,4,3]

-- > Don't try reversing an infinite fold though.

------------

-- Two more higher-order folds:

-- takingWhile   :: (a -> Bool) -> Fold s a -> Fold s a
-- droppingWhile :: (a -> Bool) -> Fold s a -> Fold s a

-- >>> [1..100] ^.. takingWhile (<3) folded
-- [1,2]
-- >>> [1..100] ^.. droppingWhile (<90) folded
-- [90,91,92,93,94,95,96,97,98,99,100]

---------

-- Exercises - Higher Order Folds

-- 1.

-- >>> "Here's looking at you, kid" ^.. dropping 7 folded
-- "looking at you, kid"
-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . taking 1 (folding words)
-- ["My","Hakuna","No"]
-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. taking 1 (folded . worded)
-- ["My"]
-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . taking 1 worded . folded
-- "MyHakunaNo"
-- >>> import Data.Char (isAlpha)
-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . takingWhile isAlpha folded
-- "MyHakunaNo"
-- >>> sumOf (takingWhile (<100) each) (10, 50, 100)
-- 60
-- >>> ("stressed", "guns", "evil") ^.. backwards each
-- ["evil","guns","stressed"]
-- >>> ("stressed", "guns", "evil") ^.. backwards each . to reverse
-- ["live","snug","desserts"]
-- >>> import Data.Char (isAlpha)
-- >>> "blink182 k9 blazeit420" ^.. worded . droppingWhile isAlpha folded
-- "1829420"

-- 2.

-- We’re doing a temperature study in a given region, but we need to run tests on several subsets of the data.
-- Here’s the series of daily temperature measurements we’ve been given for the region:

sample :: [Int]
sample = [-10, -5, 4, 3, 8, 6, -2, 3, -5, -7]

-- First we’re interested in how many days it took until the first thaw. Write an expression which calculates the number of
-- measurements until a temp is above zero:
--
-- >>> lengthOf (takingWhile (<= 0) folded) sample
-- 2

-- For the next study we need to know the warmest it got in the first 4 days of the sample.
-- Write an expression to calculate it:
--
-- >>> maximumOf (taking 4 folded) sample
-- Just 4

-- Write an expression to calculate the temperature on the day AFTER we hit that temperature.
-- Use preview or ^? somewhere in that expression if you can.
--
-- >>> sample ^? dropping 1 (droppingWhile (/= 4) folded)
-- Just 3

-- How many days of below-freezing weather did we have consecutively at the END of the sample?
--
-- >>> lengthOf (takingWhile (< 0) (backwards folded)) sample
-- 2

-- Now we’re interested in running statistics on temperature data specifically from the first thaw until
-- the next freeze. Write an expression which lists out all temperature samples from the first time we
-- sample above 0, until the next time we’re below zero.
--
-- >>> sample ^.. takingWhile (>= 0) (droppingWhile (< 0) folded)
-- [4, 3, 8, 6]

-- BONUS: List out all the temperature samples between the FIRST thaw and the FINAL freeze.
--
-- >>> sample ^.. backwards ( droppingWhile (< 0) (backwards (droppingWhile (< 0) folded)))
-- [4, 3, 8, 6, -2, 3]
--
-- Generalize this behaviour into a function: trimmingWhile.
-- It should drop elements from the start AND end of a fold while a predicate is True.
--
trimmingWhile :: (a -> Bool) -> Fold s a -> Fold s a
trimmingWhile p f = backwards ( droppingWhile p (backwards (droppingWhile p f)))
-- >>> sample ^.. trimmingWhile (< 0) folded


-----------------------------------


-- Filtering folds

-- filtered :: (s -> Bool) -> Fold s s

-- *real*
-- filtered :: (Choice p, Applicative f) => (a -> Bool) -> Optic' p f a a

-- It is extremely flexible and can be used on most types of optics.


-- >>> [1,2,3,4] ^.. folded . filtered even
-- [2,4]

-- >>> ["apple", "passionfruit", "orange", "pomegranate"] ^.. folded . filtered ((> 6) . length)
-- ["passionfruit","pomegranate"]


-- Example:

data Card =
  Card { _cardName  :: String
       , _aura  :: Aura
       , _holo  :: Bool
       , _moves :: [Move]
       } deriving (Show, Eq)

data Aura = Wet | Hot | Spark | Leafy
  deriving (Show, Eq)

data Move =
  Move { _moveName  :: String
       , _movePower :: Int
       } deriving (Show, Eq)

makeLenses ''Card
makeLenses ''Move

deck :: [Card]
deck = [ Card "Skwortul"    Wet   False [Move "Squirt" 20]
       , Card "Scorchander" Hot   False [Move "Scorch" 20]
       , Card "Seedasaur"   Leafy False [Move "Allergize" 20]
       , Card "Kapichu"     Spark False [Move "Poke" 10, Move "Zap" 30]
       , Card "Elecdude"    Spark False [Move "Asplode" 50]
       , Card "Garydose"    Wet   True  [Move "Gary's move" 40]
       , Card "Moisteon"    Wet   False [Move "Soggy" 3]
       , Card "Grasseon"    Leafy False [Move "Leaf Cut" 30]
       , Card "Spicyeon"    Hot   False [Move "Capsaicisize" 40]
       , Card "Sparkeon"    Spark True  [Move "Shock" 40, Move "Battery" 50]
       ]

-- How many Spark Cards do I Have ?
-- >>> lengthOf
--       ( folded
--       . aura
--       . filtered (== Spark) )
--       deck
-- 3

-- How many moves have an attack power above 30 ?
-- >>> lengthOf
--       ( folded
--       . moves
--       . folded
--       . movePower
--       . filtered (> 30) )
--       deck
-- 5

--  List all cards which have any move with an attack power greater than 40
-- >>> deck
--        ^.. folded
--        . filtered ( anyOf (moves . folded . movePower) (> 40) )
--        . cardName
-- ["Elecdude","Sparkeon"]

-- How many moves do my Spark cards have in total?
-- >>> lengthOf
--      ( folded
--      . filtered ((== Spark) . view aura)
--      . moves
--      . folded )
--      deck
-- 5

-- List all my Spark Moves with a power greater than 30
-- >> deck
--      ^..
--      ( folded
--      . filtered ((== Spark) . view aura)
--      . moves
--      . folded
--      . filtered ((> 30) . view movePower)
--      . moveName )



-- nb. filteredBy is from lens-4.18.0 which is really new
--
-- filteredBy :: Fold s a -> Fold s s
--
-- *more accurate signature*
-- filteredBy :: Fold s a -> IndexedTraversal' a s s
--
-- *real type*
-- filteredBy :: (Indexable i p, Applicative f) => Getting (First i) a i -> p a (f a) -> a -> f a
--
-- Filters out any element for which the given fold yields no values.



-- List all my Spark Moves with a power greater than 30
-- >>> deck
--       ^..
--       ( folded
--       . filteredBy (aura . only Spark)
--       . moves
--       . folded
--       . filtered ((> 30) . view movePower)
--       . moveName )

-- Yields a result () if and only if the input value is equal to the reference value.
--
-- only :: Eq a => a -> Fold a ()
--
-- * real signature *
-- only :: Eq a => Prism' a ()


-- >>> 1 ^? only 1
-- Just ()
-- >>> 2 ^? only 1
-- Nothing


-- Get the holographic card which has the largest number of moves
-- >>>  maximumByOf
--        ( folded . filtered _holo )
--        ( comparing (lengthOf moves) )


---------------------

-- Exercises - Filtering

-- 1.
--
-- List all the cards whose name starts with 'S'.
-- >>> deck
--       ^..
--       ( folded
--       . cardName
--       . filtered (isPrefixOf "S") )

-- What's the lower attack power of all moves ?
-- >>> minimumOf (folded . moves . folded . movePower) deck
-- Just 3

-- What's the name of the first card which has more than one move?
--  >>> firstOf
--        ( folded
--        . filtered ((> 1) . lengthOf (moves . folded))
--        . cardName )
--        deck
-- Just "Kapichu"

-- Are there any Hot cards with a move with more than 30 attack power?
-- >>> anyOf
--     ( folded
--     . filtered ((== Hot) . view aura)
--     . moves
--     . folded
--     . movePower )
--     (> 30)
--     deck
-- True


-- List the names of all holographic cards with a Wet aura
--   >>> deck
--         ^..
--         ( folded
--         . filtered _holo
--         . filtered ((== Wet) . view aura)
--         . cardName )
-- ["Garydose"]


-- What's the sum of all attack power for all moves belonging to non-Leafy cards?
-- >>  sumOf
--       ( folded
--       . filtered ((/= Leafy) . view aura)
--       . moves
--       . folded
--       . movePower )
--       deck
-- 303

---------------------

-- Fold Laws

-- Folds don't have any laws.

