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

module Operators where

-------------------------------------

import           Control.Applicative
import           Control.Lens
import           Data.Char
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as T

-------------------------------------

-- flip view == ^.
-- set == .~
-- over == %~

data Payload =
  Payload { _weight :: Int
          , _cargo  :: String
          } deriving (Show)
makeLenses ''Payload

data Ship =
  Ship { _payload :: Payload
       } deriving (Show)
makeLenses ''Ship

serenity :: Ship
serenity = Ship (Payload 50000 "Livestock")

-- >>> view (payload . cargo) serenity
-- >>> serenity ^. payload.cargo

-- >>> set (payload . cargo) "Medicine" serenity
-- >>> serenity & payload.cargo .~ "Medicine"
-- >>> serenity
--       & payload.cargo .~ "Chocolate"
--       & payload.weight .~ 2000

-- >>> serenity
--       & payload.weight %~ subtract 1000
--       & payload.cargo .~ "Chocolate"

-- ^   views/get
-- .   absence of any other modifiers
-- ^.  Gets the focus without doing anything else.

-- %   modify using a function
-- ~   updates or sets something
-- %~  modifier which returns a functions

-- +~, -~, *~, //~    add, subtract, multiply, divide  a value with the focus
-- >>> (2, 30) & _2 +~ 5

-- ^~, ^^~, **~       raise the Numeric, Fractional or Floating focus to the provided exponent.
-- >>> (2, 30) & _1 ^~ 3
-- >>> (2, 30) & _1 ^^~ 3

-- ||~, &&~   logical OR, AND of the focus with the value
-- >>> (False, 30) & _1 ||~ True

-- <>~   mappend a value onto the focus (from the right)
-- >>> ("abra", 30) & _1 <>~ "cadabra"
-- ("abracadabra",30)

---------------------------

-- Modifiers

data Thermometer = Thermometer
  { _temperature :: Int
  } deriving Show
makeLenses ''Thermometer

-- (<...) Get the altered focus (post-action) in addition to modifying it

-- >>> Thermometer 20 & temperature <+~ 15
-- (35,Thermometer {_temperature = 35})

-- (<<...) Get the OLD focus (pre-action) in addition to setting a new one
-- >>> Thermometer 20 & temperature <<+~ 15
-- (20,Thermometer {_temperature = 35})


-- When to use operators vs named actions ?
--
-- Named actions: for partially applying lens expressions
-- Operators: rest of the time

-- >>> map (view _1) [("Obi-Wan", "Kenobi"),("Anakin","Skywalker")]
-- >>> map (over _2 reverse) [("Obi-Wan", "Kenobi"),("Anakin","Skywalker")]


-- Exercises - Operators
--
-- 1.

data Gate =
  Gate { _open    :: Bool
       , _oilTemp :: Float
       }
    deriving Show
makeLenses ''Gate

data Army =
  Army { _archers :: Int
       , _knights :: Int
       }
    deriving Show
makeLenses ''Army

data Kingdom =
  Kingdom { _name :: String
          , _army :: Army
          , _gate :: Gate
          }
    deriving Show
makeLenses ''Kingdom


duloc :: Kingdom
duloc =
  Kingdom { _name = "Duloc"
          , _army = Army { _archers = 22
                         , _knights = 14
                         }
          , _gate = Gate { _open = True
                         , _oilTemp = 10.0
                         }
          }

goalA =
  duloc
    & name <>~ ": a perfect place"
    & army.knights *~ 3
    & gate.open &&~ False

goalB =
  duloc
    & name <>~ "instein"
    & army.archers -~ 5
    & army.knights +~ 12
    & gate.oilTemp **~ 2

goalC =
  duloc
    & gate.oilTemp //~ 2
    & name <>~ ": Home"
    & name <<<>~ " of the talking Donkeys"

-- 2.

-- >>> (False, "opossums") & _1 ||~ True
-- (True, "opossums")

-- >>> 2 & id *~ 3
-- 6

-- >>> import Data.Char (toUpper)
-- >>> ((True, "Dudley"), 55.0)
--          & _1 . _2 <>~ " - the worst"
--          & _2 -~ 15
--          & _2 //~ 2
--          & _1 . _2 %~ map toUpper
--          & _1 . _1 &&~ False
--
-- ((False, "DUDLEY - THE WORST"), 20.0)


-- 3.
-- (^.), (+~), (<<>~) (||~) ...

-- 4.
-- (%~) :: Lens s t a b -> (a -> b) -> s -> t
