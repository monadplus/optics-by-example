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

module PolymorphicOptics where

-------------------------------------

import           Control.Applicative
import           Control.Lens
import           Data.Char
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as T

-------------------------------------

-- Polymorphic lenses
--
--we need polymorphic lenses whenever na action might want to change the type of the focus.

-- Lens s t a b
-- <s> and <a> represent the types of the structure and focus **before** they've been acted on,
-- <t> and <b> are the structure and focus **after** being acted on.
-- type Lens' s a = s s a a


---------------------

-- Type-changing focuses

-- _1 :: Lens (a, c) (b, c) a b
-- over :: Lens s t a b -> (a -> b) -> s -> t
-- over _1 :: (a -> b) -> (a, c) -> (b, c)
-- over _1 show :: (Int, c) -> (String, c)
--
-- Our over allowed us to change the tpye of the focus, resulting in a change of type to the full structure.

-- set :: Lens s t a b -> b -> s -> t
--
-- view returns ONLY the focus and doesn't really perform any actions or modification, view just accepts a simple lens instead of a polymorphic one:
-- view :: Lens' s a -> s -> a

---------------------

-- Changing type variables with polymorphic lenses

data Promotion a =
  Promotion { _item :: a
            , _discountPercentage :: Double
            }
    deriving (Show)

item :: Lens (Promotion a) (Promotion b) a b
item = lens getter setter
  where
    getter :: Promotion a -> a
    getter = _item
    setter :: Promotion a -> b -> Promotion b
    setter promotion b = promotion{ _item = b }

-- >>> let peachPromo = Promotion "A really delicious Peach" 25.0
-- >>> set item "A smelly banana" peachPromo
-- Promotion {_item = "A smelly banana", _discountPercentage = 25.0}

--data Preferences a =
  --Preferences { _best  :: a
              --, _worst :: a
              --}
    --deriving (Show)

--best :: Lens (Preferences a) (Preferences b) a b
--best = lens getter setter
  --where
    --getter :: Preferences a -> a
    --getter = _best
    --setter :: Preferences a -> b -> Preferences b
    --setter pref b = pref{ _best = b, _worst = "What do we put here ??" }

-- It's not possible to write this lens. Since a lens focuses exactly **one** thing, and there are two things in the
-- structure with the same type variable, there's no way we can use a lens to change them both at once.

-- Spoilers: Traversal can do this.

-- Exercises - Polymorphic Lenses

-- 1. Lens (Vorpal x) (Vorpal y) x y
-- 2.

data Preferences a =
  Preferences { _best  :: a
              , _worst :: a
              }
    deriving (Show)

best :: Lens (Preferences a) (Preferences b) (a, a) (b, b)
best = lens getter setter
  where
    getter :: Preferences a -> (a, a)
    getter (Preferences a a1) = (a, a1)
    setter :: Preferences a -> (b, b) -> Preferences b
    setter pref (b, b1) = pref{ _best = b, _worst = b1 }

-- Alternative:
data Preferences2 a b =
  Preferences2 { _best2  :: a
               , _worst2 :: b
               }
    deriving (Show)

-- 3.

data Result e =
  Result { _lineNumber :: Int
         , _result     :: Either e String
         }

-- We can't focus on e because it's inside an Either and it's not always present (would break laws).
-- Instead, we need to focus on the whole Either and let the user decide.
--
-- result :: Lens (Result e) (Result f) (Either e String) (Either f String)

-- 4.

data Selector a b =
  Selector { _select :: (a, b)
           , _price  :: Int
           }

-- select :: Lens (Selector a b) (Selector c d) (a, b) (c, d)

data ParseResult e a =
    Error e
  | Result' a
  deriving Show

result :: Lens (ParseResult e a) (ParseResult f b) (Either e a) (Either f b)
result = lens getter setter
  where
    getter (Error e)  = Left e
    getter (Result' a) = Right a
    setter _ (Left e)  = Error e
    setter _ (Right a) = Result' a

-- 5. Lens to change from Predicate a to Predicate b

data Predicate a =
  Predicate (a -> Bool)

predicate :: Lens (Predicate a) (Predicate b) (a -> Bool) (b -> a)
predicate = lens getter setter
  where
    getter :: Predicate a -> (a -> Bool)
    getter (Predicate f) = f
    setter :: Predicate a -> (b -> a) -> Predicate b
    setter (Predicate f) g = Predicate (f . g)

-- Composing Lenses: nested records

data Person =
  Person { _name    :: String
         , _address :: Address
         }
    deriving (Show)

data Address =
  Address { _streetAddress :: StreetAddress
          , _city          :: String
          , _country       :: String
          }
    deriving (Show)

data StreetAddress =
  StreetAddress { _streetNumber :: String
                , _streetName   :: String
                }
    deriving (Show)

makeLenses ''Person
makeLenses ''Address
makeLenses ''StreetAddress


sherlock :: Person
sherlock =
  Person { _name    = "S. Holmes"
         , _address = Address
           { _streetAddress = StreetAddress
               { _streetNumber = "221B"
               , _streetName   = "Baker Street"
               }
               , _city    = "London"
               , _country = "England"
           }
        }

setStreetNumber :: String -> Person -> Person
setStreetNumber newStreetNumber person =
  let existingAddress       = _address person
      existingStreetAddress = _streetAddress existingAddress
   in person { _address = existingAddress
               { _streetAddress = existingStreetAddress
                  { _streetNumber = newStreetNumber
                  }
               }
             }
-- >>> setStreetNumber "220A" sherlock

-- A primary reason for this ugliness is that record updates in Haskell use special syntax for updates rather than
-- using a function, and syntax doesn't compose!

-- modifier :: (a -> a)
-- updated :: (a-> a) -> (s -> s)

---------------------

-- Composing update functions

updateAddress :: (Address -> Address)
              -> (Person -> Person)
updateAddress modify existingPerson =
  existingPerson
    { _address = modify . _address $ existingPerson
    }

updateStreetAddress :: (StreetAddress -> StreetAddress)
                    -> (Address -> Address)
updateStreetAddress modify existingAddress =
  existingAddress
    { _streetAddress = modify . _streetAddress $ existingAddress
    }

-- Update a Street Number within a Streed Address
updateStreetNumber :: (String -> String)
                   -> (StreetAddress -> StreetAddress)
updateStreetNumber modify existingStreetAddress =
  existingStreetAddress
    { _streetNumber = modify . _streetNumber $ existingStreetAddress
    }

-- >>> :t updateStreetAddress . updateStreetNumber
-- (String -> String) -> Address -> Address
-- >>> :t updateAddress . updateStreetAddress . updateStreetNumber
-- (String -> String) -> Person -> Person

-- Something interesting to notice here is that since each updater accepts an updated for a smaller piece of state,
-- if we read the composition from left to right it reads like we're diving deeper on field at a time!

-- >>> (updateAddress . updateStreetAddress . updateStreetNumber)
--       (const "221A")
--       sherlock


-- updaters = updateAddress . updateStreetAddress . updateStreetNumber
-- lenses   = address       . streetAddress       . streetNumber

-- updaters ::              (String ->   String) -> Person ->   Person
-- lenses   :: Functor f => (String -> f String) -> Person -> f Person
--
-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
-- Lens' Person String
--
-- By choosing the right Functor we can even do things that don't look like updates, like view the focus.

-- Composing lenses makes a new lens!

-- Lens composition is actually really just function composition.


data Player = Player deriving Show
data Wool = Wool deriving Show
data Sweater = Sweater deriving Show

data Item a =
  Item { _material :: a
       , _amount   :: Int
       }
  deriving Show

makeLenses ''Item

weave :: Wool -> Sweater
weave Wool = Sweater

gameState :: (Player, Item Wool)
gameState = (Player, Item Wool 5)

-- Wool into Sweater: dive into our gameState, focus the Wool, and run the weave function on it.
-- >>> over (_2 . material) weave gameState

-- Exercises - Lens Composition

-- 1.
-- >>> view (_2 . _1 . _2) ("Ginerva", (("Galileo", "Waldo"), "Malfoy"))
-- "Waldo"

-- 3.
-- Functor f => (Armadillo -> f Hedgehog) -> (Platypus -> f BabySloth)
-- Lens Platypus BabySlot Armadillo Hedgehog
