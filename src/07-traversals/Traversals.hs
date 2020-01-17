{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}
module Traversals where

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
import           Text.Read           (readMaybe)
import           Control.Applicative

-------------------------------------

-- Traversals

-- Traversals = fold + lenses
--
-- Traversals can get or set zero or more values.
--
-- All lenses are valid traversals (the opposite is not true).
-- All lenses and traversals are valid folds.
-- Folds are neither a valid lens nor traversal.
--
-- Row: what you have
-- Column: what you want
--
--              Lens    Fold     Traversal
-- Lens          X       X          X
-- Fold                  X
-- Traversal             X          X

-- nb. Traversals loses the guarantee that our optic must have a focus i.e. we can't use (^.)


-----------------------------------------------------

-- A few of the folds we've learned already have secretly been traversals!
--
-- both, each, filtered, filteredBy, taking, dropping, takingWhile, droppingWhile, ...

-----------------------------------------------------

-- From fold to traversal


-- both :: Traversal (a, a) (b, b) a b
--
-- *real types*
-- both :: Bitraversable r => Traversal (r a a) (r b b) a b


-- >>> ("Bubbles", "Buttercup") ^.. both
-- ["Bubbles", "Buttercup"]


-- >>> ("Bubbles", "Buttercup") & both <>~ "!"
-- ("Bubbles!", "Buttercup!")

-- >>> ("Bubbles", "Buttercup") & both .~ "Blossom"
-- ("Blossom","Blossom")

-- >>> ("Bubbles", "Buttercup") & both %~ length
-- (7, 9)

-- (1, 2, 3) & each *~ 10
-- (10, 20, 30)

-- [1, 2, 3] & each *~ 10
-- [10, 20, 30]

-----------------------------------------------------

-- A polymorphic traversal has to change ALL occurences of a type variable within  the structure in order for the type to change.

-- Some structures like Text or ByteString are *monomorphic structures* and can't change types.

-----------------------------------------------------

-- takingWhile, droppingWhile:

-- >>> [1, 2, 3, 4, 5] & taking 3 traversed *~ 10
-- [10,20,30,4,5]

-- >>> [1, 2, 3, 4, 5] & dropping 3 traversed *~ 10
-- [1,2,3,40,50]

-- >>> "once upon a time - optics became mainstream"
--       & takingWhile (/= '-') traversed
--       %~ toUpper
-- "ONCE UPON A TIME - optics became mainstream"


-- filtered:
--
-- Multiply all even numbers by 10
-- >>> [1,2,3,4,5]
--       & traversed . filtered even
--       *~ 10
-- [1,20,3,40,5]

-- Reverse only the long strings
-- ("short", "really long"
--   & each . filtered ((> 5) . length)
--   %~ reverse

-----------------------------------------------------

-- Traversal Combinators

-- Traversing each element of a container

-- If we try to set or update using `folded` GHC will yell at us:
-- >>> [1,2,3] & folded %~ (*10)
-- <interactive>:107:11: error:
--  • Could not deduce (Contravariant Identity)
--      arising from a use of ‘folded’


-- traversed :: Traversable f => Traversal (f a) (f b) a b
--
-- *real*
-- traversed :: Traversable f => IndexedTraversal Int (f a) (f b) a b


-- Requiring Traversable as opposed to only Foldable gives us a bit more power, but also reduces the number of containers we can operat on. For instance, Sets are Foldable but not Traversable.

-- >>> [1,2,3] & traversed *~ 10
-- [10,20,30]

-- Tuples are traversable over their last slot
-- >>> ("Batman", "Superman") & traversed %~ take 3
-- ("Batman","Sup")

-- We can traverse all values in a Map
-- >>> let powerLevels =
--       Map.fromList [ ("Gohan", 710)
--                    , ("Goku", 9001)
--                    , ("Krillin", 5000)
--                    , ("Piccolo", 408)
--                    ]
-- >>> powerLevels
--       & traversed %~ \n ->
--           if n > 9000
--             then "Over 9000"
--             else show n
--
-- fromList [("Gohan","710"),("Goku","Over 9000"),("Krillin","5000"),("Piccolo","408")]


-- We can traverse over more complex structures like Trees:
-- >>> import Data.Tree
-- >>> let opticsTree = Node "Lens" [Node "Fold" [], Node "Traversal" []]
-- >>> opticsTree & traversed %~ reverse
-- Node {rootLabel = "sneL", subForest = [Node {rootLabel = "dloF", subForest = []},Node {rootLabel = "lasrevarT", subForest = []}]}

--------------------------------

-- More Combinators

-- worded :: Traversal' String String
-- lined  :: Traversal' String string

-- *real types*
-- worded :: Applicative f => IndexedLensLike' Int f String String
-- lined  :: Applicative f => IndexedLensLike' Int f String string

-- >>> "one two three" ^.. worded
-- ["one","two","three"]
-- >>> "one\ntwo\nthree" ^.. lined
-- ["one","two","three"]

-- >>> "blue suede shoes" & worded %~ \s -> "*" ++ s ++ "*"
-- "*blue* *suede* *shoes*"
--
-- >>> "blue suede shoes" & (worded.taking 1 traversed) %~ toUpper
-- >>> "blue suede shoes" & worded %~ \(x:xs) -> (toUpper x) : xs
-- "Blue Suede Shoes"


-- >>> "blue\nsuede\nshoes" & lined %~ ('#' :)
-- "#blue\n#suede\n#shoes"



-- Be careful with worded and lined (these traversals are technically unlawful).
-- They rebuild to the result string by effectively calling 'unwords' or 'unlines'
--
-- >>> "blue \n suede \n \n shoes" & worded %~ id
-- "blue suede shoes"


----------------------------------

-- Traversing multiple paths at once


-- beside :: Traversal s t a b
--        -> Traversal s' t' a b
--        -> Traversal (s, s') (t, t') a b
--
-- beside :: Lens s t a b -> Lens s' t' a b -> Traversal (s, s') (t, t') a b
-- beside :: Fold s a     -> Fold s' a      -> Fold (s, s') a
-- Works on any Bitraversable type.


-- >>> ("T-Rex", (42, "Stegosaurus")) ^.. beside id _2
-- ["T-Rex","Stegosaurus"]
--
-- >>> ([(1,2),(3,4)], [5,6,7]) ^.. beside (traversed . both) traversed
-- [1,2,3,4,5,6,7]



-- both = beside id id



-- >>>> ("Cowabunga", ["let's", "order", "pizza"])
--        & beside traversed (traversed . traversed)
--        %~ toUpper
--
-- ("COWABUNGA",["LET'S","ORDER","PIZZA"])

-- >>> Left (1, 2) & beside both traversed %~ negate
-- Left (-1,-2)
-- >>> Right [3, 4] & beside both traversed %~ negate :: Either (Int, Int) [Int]
-- Right [-3,-4]


----------------------------------------------

-- Focusing a specific traversal element

-- Traverse just a single specific element ?

-- element :: Traversable f => Int -> Traversal' (f a) a

-- It is a monomorphic traversal (because it doesn't focus every element of the container).

-- >>> [0,1,2,3,4] ^? element 2
-- Just 2
-- >>> [0,1,2,3,4] & element 2 *~ 100
-- [0,1,200,3,4]


-- elementOf :: Traversal' s a -> Int -> Traversal' s a
-- elementOf :: Fold s a       -> Int -> Fold s a


-- >>> [0,1,2,3,4] ^? elementOf traversed 2
-- Just 2
-- >>>> [[0,1,2], [3,4], [5,6,7,8]] ^? elementOf (traversed . traversed) 6
-- Just 6
-- >>> foo = [[0,1,2], [3,4], [5,6,7,8]]
--             & elementOf (traversed . traversed) 6 *~ 100
-- [[0,1,2],[3,4],[5,600,7,8]]


------------------------------------------------

-- Traversal Composition


-- Capitalize the first char of every word
-- >>> "blue suede shoes" & worded . taking 1 traversed %~ toUpper
-- "Blue Suede Shoes"

-- Find all strings longer than 5 chars then surround each word in the string with '*'
-- >>> ["short", "really long"]
--        & traversed
--        . filtered ((> 5) . length)
--        . worded
--        %~ (\s -> "*" ++ s ++ "*")
-- ["short", "*really* *long*]

-- Add "Rich" to the names of people with more than $1000
-- >>> (("Ritchie", 100000), ("Archie", 32), ("Reggier", 4350))
--       & each
--       . filtered ((> 1000) . snd)
--       . _1
--       %~ ("" ++)
-- (("Rich Ritchie", 100000), ("Archie", 32), ("Rich Reggier", 4350))


---------------------------------------------

-- Exercises - Simple Traversals

-- 1.
-- - fold
-- - lens (and traversal)
-- - lens, traversal (and fold)

-- 2.

-- >>> ("Jurassic", "Park") & both .~ "N/A"
-- ("N/A", "N/A")
--
-- >>> ("Jurassic", "Park") & both . traversed .~ 'x' :: (String, String)
-- ("xxxxxxxx", "xxxx")
--
-- >>> ("Malcolm", ["Kaylee", "Inara", "Jayne"])
--        & beside id traversed
--        %~ take 3
-- ("Mal", ["Kay", "Ina", "Jay"])
--
-- >>> ("Malcolm", ["Kaylee", "Inara", "Jayne"])
--        & _2
--        . element 1
--        .~ "River"
-- ("Malcolm", ["Kaylee", "River", "Jayne"])
--
-- -- This one's tricky!
-- >>> ["Die Another Day", "Live and Let Die", "You Only Live Twice"]
--         & traversed . elementOf worded 1 . traversed
--         .~ 'x'
-- [ "Die xxxxxxx Day"
-- , "Live xxx Let Die"
-- , "You xxxx Live Twice"
-- ]
--
-- -- A bit tougher now!
-- >>> ((1, 2), (3, 4)) & each . both +~ 1
-- ((2, 3), (4, 5))
--
-- >>> (1, (2, [3, 4])) & beside id (beside id traversed) +~ 1
-- (2, (3, [4, 5]))
--
-- >>> import Data.Char (toUpper)
-- >>> ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries"))
--       & each
--       . filtered fst
--       . _2
--       . takingWhile (/= 'b') traversed
--       %~ toUpper
-- ((True, "STRAWberries"), (False, "Blueberries"), (True, "BLACKberries"))

-- >>> foo = ((True, "Strawberries"::String), (False, "Blueberries"), (True, "Blackberries"))
--              & each
--              %~ snd
-- ("Strawberries", "Blueberries", "Blackberries")


-------------------------------------------

-- Traversal Actions

-- traverseOf :: Traversal s t a b -> (a -> f b) -> s -> f t
--
-- * real signature *
-- traverseOf :: LensLike f s t a b -> (a -> f b) -> s -> f t

-- traverseOf: runs traverse over the focuses of a traversal.
-- nb. traverseOf traversed = traverse

-- >>> import Text.Read (readMaybe)
-- >>> traverseOf both readMaybe ("1", "2") :: Maybe (Int, Int)
-- Just (1, 2)
-- >>> traverseOf both readMaybe ("not a number", "2") :: Maybe (Int, Int)
-- Nothing

-- Cartesian-product
-- >>> traverseOf both (\c -> [toLower c, toUpper c]) ('a', 'b')
-- [('a','b'),('a','B'),('A','b'),('A','B')]



-- Since we can compose Traversals just like other optics we can get pretty wild with the idea.



-- The Applicative for the list effect will find All possible combinatins of each branch of the traversal tree, so we end up with something like this:
--
-- >>> traverseOf
--       (both . traversed)
--       (\c -> [toLower c, toUpper c])
--       ("ab"::String, "cd")
--
-- [("ab","cd"),("ab","cD"),("ab","Cd"),("ab","CD"),("aB","cd"),("aB","cD"),("aB","Cd"),("aB","CD"),("Ab","cd"),("Ab","cD"),("Ab","Cd"),("Ab","CD"),("AB","cd"),("AB","cD"),("AB","Cd"),("AB","CD")]

-- The result is a list of all possible copies of the structure given that every character may be capitalized or not.



validateEmail :: String -> Either String String
validateEmail email | elem '@' email = Right email
                   | otherwise      = Left ("missing '@': " <> email)

-- >>> traverseOf (traversed . _2) validateEmail
--        [ ("Mile", "mike@tmnt.io")
--        , ("Raph", "raph@tmnt.io")
--        ]
-- Right [("Mile","mike@tmnt.io"),("Raph","raph@tmnt.io")]


-- We can use Data.Either.Validation (from either package) to accumulate the errors.



-- forOf :: Traversal s t a b -> s -> (a -> f b) -> f t
-- forOf = flip traverseOf

-- sequenceAOf :: Traversal s t (f a) a -> s -> f t

-- >>> sequenceAOf _1 (Just "Garfield", "Lasagna")
-- Just ("Garfield", "Lasagna")
-- >>> sequenceAOf (both . traversed) ([Just "apples"], [Just "oranges"])
-- Just (["apples"],["oranges"])
-- >>> sequenceAOf (both . traversed) ([Just "apples", Nothing], [Just "oranges"])
-- Nothing

----------------------------

-- Infix traverseOf

-- traverseOf :: Traversal s t a b -> (a -> f b) -> s -> f t
--
-- (%%~)      :: Traversal s t a b -> (a -> f b) -> s -> f t

-- >>> (("1", "2") & both %%~ readMaybe) :: Maybe (Int, Int)
-- Just (1,2)
-- >>> (("totally not a number", "2") & both %%~ readMaybe) :: Maybe (Int, Int)
-- Nothing

----------------------------

-- Using Traversals directly (not recommended)

-- The _lens_ library's Van Laarhoven encoding menas that most Traversal's can be used as-is as a custom traverse function. Instead of using traverseOf or %%~, we can often use the traverse itself!

-- >>> both readMaybe ("1", "2") :: Maybe (Int, Int)
-- Just (1, 2)

-- This is possible because of the definition of the Traversal type in the lens library:
--
-- type Traversal st a b = forall f. Applicative f => (a -> f b) -> s -> f t

-- The fact that Traversal match the traverse functions means that lens library implementation
-- of traverseOf and %%~ looks like this:
--
-- traverseOf :: LensLike f s t a b -> (a -> f b) -> s -> f t
-- traverseOf = id

-----------------------------

-- Exercises - Traversal Actions

-- 1.

-- >>> sequenceAOf _1 (Nothing, "Rosebud")
-- Nothing
--
-- >>> sequenceAOf (traversed . _1) [(['a','b'], 1), (['c','d'], 2)]
-- [ [('a', 1) ,('c', 2)]
-- , [('a', 1) ,('d', 2)]
-- , [('b', 1) ,('c', 2)]
-- , [('b', 1) ,('d', 2)]]
--
-- The ZipList effect groups elements by position in the list
-- >>> sequenceAOf traversed [ZipList [1, 2], ZipList [3, 4]]
-- ZipList {getZipList = [[1,3],[2,4]]}
--
-- >>> sequenceAOf (traversed . _2) [('a', ZipList [1,2]), ('b', ZipList [3,4])]
-- ZipList {getZipList = [[('a',1),('b',3)],[('a',2),('b',4)]]}
--
-- >>> import Control.Monad.State
-- >>> let result = traverseOf
--                     (beside traversed both)
--                     (\n -> modify (+n) >> get)
--                     ([1, 1, 1], (1, 1))
-- >>> runState result 0
-- (([1,2,3],(4,5)), 5)

-- 2. Rewrite using infix operator

-- >>> ("ab", True) & (_1 . traversed) %%~ (\c-> [toLower c, toUpper c])
-- [("ab",True),("aB",True),("Ab",True),("AB",True)]

-- >>> [('a', True), ('b', False)] & (traversed . _1) %%~ (\c-> [toLower c, toUpper c])
-- [[('a',True),('b',False)],[('a',True),('B',False)],[('A',True),('b',False)],[('A',True),('B',False)]]

-- 3.

data User = User
  { _name :: String
  , _age :: Int
  } deriving Show

makeLenses ''User

data Account = Account
  { _idAccount :: String
  , _user      :: User
  } deriving Show

makeLenses ''Account

-- Validate that the given user has an age above zero and below 150
validateAge :: Account -> Either String Account
validateAge = traverseOf (user.age) check
  where
    check age
      | age > 0 && age < 150 = Right age
      | otherwise = Left ("Wrong age: " ++ show age)

-------------------------------------------

-- Custom traversals

-- Van Laarhoven optics all look something like this:
--
-- type LensLike f s t a b = (a -> f b) -> (s -> f t)

-- All of the optics we've learned so far can be written as some kind of LensLike!
-- Different optics have differing constraints on the effect type <f>.

-- LensLike form:
-- type Lens s t a b      = forall f. Functor f => LensLike f s t a b
-- type Traversal s t a b = forall f. Applicative f => LensLike f s t a b
-- type Fold s a          = forall f (Contravariant f, Applicative f) => LensLike f s t a b

-- Var Laarhoven form:
-- type Lens s t a b      = forall f. Functor f => (a -> f b) -> (s -> f t)
-- type Traversal s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)
-- type Fold s a          = forall f (Contravariant f, Applicative f) => (a -> f b) -> (s -> f t)

-- The symmetry we see in these shapes is why we can compose optics of different types together. Every optic is actually _the exact same type_ plus or minus constraints on <f>.
--
-- Most optics are really just traverse wearing different pants.


-------------------

-- Our first custom traversal

-- Let's implement a simpliifed version of traversed which works only on lists:
--
-- values :: Traversal [a] [b] a b

values :: Applicative f => (a -> f b) -> [a] -> f [b]
values _ [] = pure []
values f (x:xs) =  liftA2 (:) (f x) (values f xs)

-- >>> ["one", "two", "three"] ^.. values
-- ["one","two","three"]

-- >>> ["one", "two", "three"] & values %~ reverse
-- ["eno","owt","eerht"]

-- We should be able to do type-changing transformations too:
-- ["one", "two", "three"] & values %~ length
-- [3,3,5]

-------------------------

-- Traversals with custom logic

data Transaction =
    Withdrawal { _amount :: Int}
  | Deposit    { _amount :: Int}
  deriving Show
makeLenses ''Transaction
-- ^^^^^^^^^^^^^^^ this works because it's generating: amount :: Lens' Transaction Int

newtype BankAccount = BankAccount
  { _transactions :: [Transaction]
  } deriving Show
makeLenses ''BankAccount
-- transactions :: Lens' BankAccount [Transaction]

-- In reality makeLenses actually generates an iso:
-- transactions :: Iso' BankAccount [Transaction]


-- >>> let aliceAccount = BankAccount [Deposit 100, Withdrawal 20, Withdrawal 10]

-- Get all transactions
-- >>> aliceAccount ^.. transactions . traversed
-- [Deposit {_amount = 100},Withdrawal {_amount = 20},Withdrawal {_amount = 10}]

-- Get the amounts for all transactions
-- >>> aliceAccount ^.. transactions . traversed . amount
-- [100,20,10]

-----------------------

-- Case Study: Transaction Traversal

-- We need a traversal which focuses on only the dollar amounts of deposits whitin a given account.
--
-- deposits :: Traversal' [Transaction] Int
-- deposits :: Traversal [Transaction] [Transaction] Int Int
-- deposits :: Applicative f => (Int -> f Int) -> [Transaction] -> f [Transaction]

deposits :: Traversal' [Transaction] Int
deposits _ [] = pure []
deposits f ((Withdrawal n) : xs) = liftA2 (:) (pure (Withdrawal n)) (deposits f xs)
deposits f ((Deposit n) : xs) = liftA2 (:) (Deposit <$> f n) (deposits f xs)

-- This form is recommended. Sometimes you need to write it manually though.
deposits2 :: Traversal' [Transaction] Int
deposits2 = traversed . filtered isDeposit . amount
  where
    isDeposit (Deposit _) = True
    isDeposit _ = False

--Get all the Deposit transaction amounts:
-- >>> aliceAccount ^.. transactions . deposits
-- [1000]

-- Multiply the amounts of all Deposits by 10
-- >>> aliceAccount & transactions . deposits *~ 10
-- BankAccount {_transactions = [Deposit {_amount = 1000},Withdrawal {_amount = 20},Withdrawal {_amount = 10}]}


--------------------------------

-- Exercises - Custom Traversals

-- 1.

amountT :: Traversal' Transaction Int
amountT f (Withdrawal n) = Withdrawal <$> f n
amountT f (Deposit n   ) = Deposit <$> f n

-- 2.

both' :: Traversal (a, a) (b, b) a b
both' f (a1, a2) = liftA2 (,) (f a1) (f a2)

-- 3.

transactionDelta :: Traversal' Transaction Int
transactionDelta f = \case
  Withdrawal n -> Withdrawal . negate <$> f (- n)
  Deposit n    -> Deposit    <$> f n

-- Here's how it should behave:
--
-- >>> Deposit 10 ^? transactionDelta
-- Just 10

-- Withdrawal's delta is negative
-- >>> Withdrawal 10 ^? transactionDelta
-- Just (-10)
--
-- >>> Deposit 10 & transactionDelta .~ 15
-- Deposit {_amount = 15}
--
-- >>> Withdrawal 10 & transactionDelta .~ (-15)
-- Withdrawal {_amount = 15}
--
-- >>> Deposit 10 & transactionDelta +~ 5
-- Deposit {_amount = 15}
--
-- >>> Withdrawal 10 & transactionDelta +~ 5
-- Withdrawal {_amount = 5}

-- 4.
--
left :: Traversal (Either a b) (Either a' b) a a'
left f (Left a)  = Left <$> f a
left _ (Right b) = Right <$> pure b
-- >>> Left 10 & left %~ (+2)
-- >>> Right "hello" & left %~ (+2)

-- 5.
-- Hint: You can use traverseOf or %%∼ to help simplify your implementation!

beside' :: Traversal s t a b
      -> Traversal s' t' a b
      -> Traversal (s,s') (t,t') a b
-- (a -> f b) -> (s, s') -> f (t, t')
beside' f g h (s, s') =
  liftA2 (,)
  (s  & f %%~ h) -- f t
  (s' & g %%~ h) -- f t'

-- >>> (1, (2, [3, 4])) & beside' id (beside' id traversed) +~ 1
-- (2,(3,[4,5]))

--------------------------

-- Traversal Laws


-- Law One: Respect Purity
-- traverseOf myTraversal pure x == pure x

-- Checks that traversal is not adding any affects or altering ay data on its own.

-- >>> traverseOf both pure ("don't", "touch") :: [(String, String)]
-- >>> pure ("don't", "touch") :: [(String, String)]
-- [("don't","touch")]

-- nb. since the implementation is completly polymorphic over the Applicative we know that if *it passes the law for one Applicative it must pass for all of them*.

badTupleSnd :: Traversal (Int, a) (Int, b) a b
badTupleSnd handler (n, a) = liftA2 (,) (pure (n + 1)) (handler a)
-- >>> traverseOf badTupleSnd pure (10, "Yo")
-- (11, "Yo")
-- >>>> pure (10, "Yo")
-- (10, "Yo")


------

-- Law Two: Consistent Focuses

-- Running a handler over a traversal should *never change which elements are focused* in the traversal. Running a traversal twice in a row with different handlers should be equivalent to running it once with hte cmpositio nof those handlers (Applicative compose).

-- fmap (traverseOf myTrav f) . traverseOf myTrav g $ x
--  ==
-- getCompose . traverseOf myTrav (Compose . fmap f . g) $ x


-- import Data.Functor.Compose ( Right-to-left composition of functors. The composition of applicative functors is always applicative, but the composition of monads is not always a monad.)


-- >>> (0,0) & both %~ (+10)
--           & both %~ (+10)
-- (100,100)
-- >>> (0,0) & both %~ (+10) . (+10)
-- (100,100)


-- Property-based testing would help checking those laws:
--  - QuickCheck
--  - Hedgehog

-- filtered is a law-breaking traversal!
-- >>> 2 & filtered even +~ 1
--       & filtered even *~ 10
-- 3
-- >>> 2 & filtered even %~ (*10) . (+1)
-- 30


-- Some traversals like filtered are very useful but can't be encoded in a legal way.


---------------------------

-- Exercises - Traversal Laws

-- 1. worded is a law-breaking traversal!
--    The second law, if you put a whitespace, it changes the focus.

-- >>> traverseOf worded pure "one two three" :: Maybe String
-- "one two three"
-- >>> "one two three" & worded <>~ " missisipi"
--                     & worded %~ reverse
-- >>> "one two three" & worded %~ reverse . (<> " missisipi")

-- 2.


badValues :: Applicative f => (a -> f b) -> [a] -> f [b]
badValues _ [] = pure []
badValues f xs = go False xs
  where
    go _     []     =  pure []
    go True  (x:xs) =  go False xs
    go False (x:xs) =  liftA2 (:) (f x) (go True xs)

-- >>> traverseOf badValues pure [1..10] :: Maybe [Int]
-- >>> pure [1..10] :: Maybe [Int]

-- 3.

badValues' :: (Applicative f, a ~ Int) => (a -> f a) -> [a] -> f [a]
badValues' _ []     = pure []
badValues' f (x:xs) = if even x
                        then liftA2 (:) (f x)    (badValues' f xs)
                        else liftA2 (:) (pure x) (badValues' f xs)
-- First law OK
-- >>> traverseOf badValues' pure [1..10] :: Maybe [Int]
-- >>> pure [1..10] :: Maybe [Int]
-- Second law KO
-- >>> [1..10] & badValues' %~ (+1)
--             & badValues' %~ ((+) (-1))
-- >>> [1..10] & badValues' %~ ((+)(-1) . (+1))

-- 4. Lawful or not ?
-- taking is lawful (if the traversal passed is lawful)
-- beside is lawful
-- each: is lawful
-- lined is unlawful like worded
-- traversed is lawful
