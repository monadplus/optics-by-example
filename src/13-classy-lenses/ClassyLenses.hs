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
{-# LANGUAGE FunctionalDependencies #-}
module ClassyLenses where

-------------------------------------

import           Control.Applicative
import           Control.Applicative
import           Control.Lens        as L
import           Control.Monad.State
import           Control.Monad.Reader
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

-- Classy Lenses is a **design pattern** that solves one of the following design needs:
-- * Polymorphism over specific record fields
-- * Separating layers of logic without cross-dependencies
-- * Isolating the 'knowledge' of a given module of code

---------------

-- No duplicate record fields

data Person =
  Person { _personName :: String
         } deriving Show

data Pet =
  Pet { _petName :: String
      } deriving Show


greetPerson :: Person -> IO ()
greetPerson p = putStrLn $ "Hello " <> _personName p <> "!"

greetPet :: Pet -> IO ()
greetPet p = putStrLn $ "Hello " <> _petName p <> "!"

-- We can do better, instead of writing a specialized version for each record.

-- :set -XFunctionalDependencies
makeFields ''Person
makeFields ''Pet


-- :browse ClassyLenses
--
-- class HasName s a | s -> a where
--   name :: Lens' s a
--   {-# MINIMAL name #-}
-- instance HasName Person String
-- instance HasName Pet String

greetByName :: HasName r String => r -> IO ()
greetByName r = putStrLn $ "Hello " <> r ^. name <> "!"
-- >>> greetByName (Person "Kelvin")
-- >>> greetByName (Pet "Darwin")



-- makeFields will look for the appropiate Has* class in scope, if it exists already it will
-- just implement an instance. If it can't find an existing instance it will both define the class AND
-- implement an instance.
--
-- This means that if you have two isolated modules which each define HasName classes but neither of the
-- imports each-other, that they'll inadvertently define two separate and incompatible versions of the HasName.
--
-- You can define a separate module just for that field which calls makeFields on a dummy record, then export
-- only the field typeclass for other modules to import.


------------------------------------------------------------

-- Separating logic and minimizing global knowledge

-- We already know that MonadReader/MonadState/MonadWriter have a functional dependency which
-- doesn't allow us to change the environment.

type DatabaseUrl = String

data Env =
  Env { _portNumber  :: Int
      , _hostName    :: String
      , _databaseUrl :: DatabaseUrl
      } deriving Show
makeLenses ''Env

connectDB :: (MonadIO m, MonadReader Env m) => m ()
connectDB = do
  url <- view databaseUrl
  liftIO $ putStrLn ("connecting to db at: " <> url)

initialize :: (MonadIO m, MonadReader Env m) => m ()
initialize = do
  port <- view portNumber
  host <- view hostName
  liftIO $ putStrLn ("initializing server at: " <> host <> ":" <> show port)

main :: IO ()
main = do
  flip runReaderT (Env 8000 "example.com" "db.example.com") $ do
    initialize
    connectDB

-- One problem is that there is a very *rigid dependency* on our Env type, if that type changes
-- significanlty we'll have to fix code across our entire app.

-- Another problem is that each of our actions depends on the entirety of our environment all the time.
-- We don't have isolation guarantees, and none of the type signatures for our actions clearly communicate their
-- dependencies.

-- These 'global' dependencies can either be really convenient or terribly dangerous depending on your organization, size of app, privacy/security...

-- This is a possible solution (using magnify / zoom):

connectDB' :: (MonadIO m, MonadReader DatabaseUrl m) => m ()
connectDB' = do
  url <- ask
  liftIO $ putStrLn ("connecting to db at: " <> url)

main' :: IO ()
main' =
  flip runReaderT (Env 8000 "example.com" "db.example.com") $ do
  initialize
  (magnify databaseUrl connectDB')

-- We can do better!

------------------------------------------------------------

-- Granular dependencies with makeFields


-- DB.hs (note the dummy DBFields):


{-
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module DB (HasDatabaseUrl(..), connectDB) where

import Control.Lens
import Control.Monad.Reader

type DatabaseUrl = String

data DbFields = DbFields {_dbFieldsDatabaseUrl :: DatabaseUrl}

makeFields ''DbFields

type DatabaseUrl = String
connectDB :: ( MonadIO m, HasDatabaseUrl e DatabaseUrl
             , MonadReader e m
             )
          => m ()
connectDB = do
  url <- view databaseUrl
  liftIO $ putStrLn ("connecting to db at: " <> url)
-}


-- ^^^^^^^^^^ Note that we did not export DbFields or it's record accessors at all!
-- We could use makeFieldsNoPrefix:
--   data DbFields = DbFields { _databaseUrl :: String }
--   makeFieldsNoPrefix ''DbFields



{-
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
module Main where

import Control.Lens
import DB

data Env =
  Env { _envPortNumber :: Int
      , _envHostName :: String
      , _envDatabaseUrl :: String
      } deriving Show

makeFields ''Env
-}



-- ^^^^^ We do have to import the DB module just to get the instances in scope, otherwise
-- makeFields will define new classes for each of its fields.

-- Now our DB module can be shared amongst any number of services which simply have to add the
-- appropiate makeFields calls o ntheir Env types in order to use it.


--------------------------------------------------

-- Field requirements compose


-- A benefit of field typeclasses is that they're specified with *constraints*, and constraints *compose*!

-- Here is an example:


{-
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
module Init (HasHostName(..), HasPortNumber(..)) where

import Control.Lens
import Control.Monad.Reader

data InitFields =
  InitFields { _hostName :: String
             , _portNumber :: Int
             }

makeFieldsNoPrefix ''InitFields

initialize :: ( MonadIO m
              , HasHostName e String
              , HasPortNumber e Int
              , MonadReader e m
              )
           => m ()
initialize = do
  port <- view portNumber
  host <- view hostName
  liftIO $ putStrLn ("initializing server at: " <> host <> ":" <> show port)
-}



---------------------------------------------------

-- makeFields vs makeClassy


-- There are a half-dozen different ways to generate lenses using TemplateHaskell.
--
-- This chapter covers the distinction between makeFields and makeClassy

{-
module People where
-- ...

data Person' =
  Person' { _name          :: String
          , _favouriteFood :: String
          } deriving Show

makeFieldsNoPrefix ''Person
-}

-- >>> :browse People
--
-- class HasName s a | s -> a where
--   name :: Lens' s a
--   {-# MINIMAL name #-}


-- class HasFavouriteFood s a | s -> a where
--   favouriteFood :: Lens' s a
--   {-# MINIMAL favouriteFood #-}


-- Let's try with makeClassy


{-
module People where
-- ...

data Person' =
  Person' { _name          :: String
          , _favouriteFood :: String
          } deriving Show

makeClassy ''Person
-}



-- class HasPerson c where
--   person :: Lens' c Person
--   favouriteFood :: Lens' c String
--   name :: Lens' c String
--   {-# MINIMAL person #-}


-- Let's re-factor our Database Config example for earlier to use makeClassy:


{-
module DBClassy (DbConfig(..), HasDbConfig(..), connectDB) where

import Control.Lens
import Control.Monad.Reader

data DbConfig =
  DbConfig { _databaseUrl    :: String
           , _maxConnections :: Int
           } deriving Show

makeClassy ''DbConfig

connectDB :: (MonadIO m, HasDbConfig e, MonadReader e m) => m ()
connectDB = do
  url <- view databaseUrl
  numConnections <- view maxConnections
  liftIO
    $ putStrLn ( "connecting to db at: "
                  <> url
                  <> " with max connections: "
                  <> show numConnections
               )
-}



-- Differences:
-- * Only one constraint: HasDbConfig  (which makes no mention of specific fields)
-- * We export the DBConfig alongisde the typeclass since anyone implementing the typeclass will
--   pressumably need to have a DBConfig nested in their object somewhere.

-- Here's how it looks when we integrate this into our Env:

{-
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
module EnvClassy where

import Control.Lens
import Control.Monad.Reader
import DBClassy

data Env =
  Env { _envDbConfig :: DBConfig
      } deriving Show
-}

-- If you try using makeFields ''Env:

-- makeFields ''Env
-- ^^^^^^^^^
--
--   • Expected kind ‘* -> Constraint’,
--       but ‘HasDbConfig Env’ has kind ‘Constraint’
--   • In the instance declaration for ‘HasDbConfig Env DbConfig’
--    |
-- 16 | makeFields ''Env


-- The solution:

{-
data Env =
  Env { _envDbConfig :: DBConfig
      } deriving Show

makeLenses ''Env

instance HasDbConfig Env where
  dbConfig = envDbConfig
-}


-- We can now use each of the HasDbConfig fiel lenses directly on an Env!

-- >>> let env = Env (DbConfig "db.example.com" 100)

-- >>> env ^. databaseUrl
-- "db.example.com"
--
-- >>> env ^. maxConnections
-- 100


------------------------

-- Chose what suits you better (none is better):

-- * makeFields: reducing the annoyanes of records with shared names and for implement field-polymorphic functions.

-- * makeClassy: scale a little better.
