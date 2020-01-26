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
module OpticsAndMonads where

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
import           Text.Printf

-------------------------------------

-- Reader Monad and View


type UserName = String
type Password = String

data Env =
  Env { _currentUser :: UserName
      , _users       :: Map UserName Password
      }
    deriving Show
makeLenses ''Env

--printUser :: ReaderT Env IO ()
--printUser = do
  --user <- asks _currentUser
  --liftIO . putStrLn $ "Current user: " <> user

main :: IO ()
main = runReaderT printUser (Env "jenkins" (Map.singleton "jenkins" "huner2"))

printUser :: ReaderT Env IO ()
printUser = do
  user <- view currentUser
  liftIO . putStrLn $ "Current user: " <> user
  -- ^^^ This is actually the same view function.

-- view :: MonadReader s m => Getting a s a -> m a

-- Function s -> a is a valid monad reader .
-- MonadReader s m, m ~ ((->) s) => m a

-- Fun fact, preview works the same way!

getUserPassword :: ReaderT Env IO ()
getUserPassword = do
  userName <- view currentUser
  maybePassword <- preview (users . ix userName)
  liftIO $ print maybePassword

main' :: IO ()
main' = runReaderT getUserPassword (Env "jenkins" (Map.singleton "jenkins" "hunter2"))
-- Just "huner2"

------------------------------------------------------

-- State Monad Combinators

-- Almost ALL setter combinators have State equivalents which simply replace the ~ with an =

-- import Control.Monad.State

data Till =
  Till { _total   :: Double
       , _sales   :: [Double]
       , _taxRate :: Double
       } deriving Show

makeLenses ''Till

saleCalculation :: StateT Till IO ()
saleCalculation = do
  total .= 0
  total += 8.55
  total += 7.36
  totalSale <- use total
  liftIO $ printf "Total sale: $%.2f\n" totalSale
  sales <>= [totalSale]
  total <~ uses taxRate (totalSale *)
  taxIncluded <- use total
  liftIO $ printf "Tax included: $%.2f\n" taxIncluded
-- >>> execStateT saleCalculation (Till 0 [] 1.11)
-- Total sale: $15.91
-- Tax included: $17.66
-- Till {_total = 17.660100000000003, _sales = [15.91], _taxRate = 1.11}<Paste>

-- (.=) :: MonadStates m=> Lens s s a b -> b -> m ()
-- use :: MonadStates m=> Lens's a -> m a
-- (<>=) = <>~ but for MonadState
-- uses :: MonadState s m => Lens' s a -> (a -> r) -> m r

-- (<~) :: MonadState s m => Lens s s a b -> m b -> m b -> m ()
-- ^^^^ doesn't come up very often, but it can save a little boilerplate.


-- All of these MonadState combinators have alternate versions which return the existing
-- or altered versions of the focus, see (<+=), (<<+=), (<<~), etc


--------------------------------------------------------

-- Magnify & Zoom

-- magnify :: Lens' s a -> ReaderT a m r -> ReaderT s m r

data Weather =
  Weather { _temperature :: Float
          , _pressure    :: Float
          }
  deriving Show

makeLenses ''Weather

printData :: String -> ReaderT Float IO ()
printData stateName = do
  num <- ask
  liftIO . putStrLn $ stateName <> ": " <> show num

weatherStats :: ReaderT Weather IO ()
weatherStats = do
  magnify temperature (printData "temperature")
  magnify pressure (printData "pressure")
-- >>> runReaderT  weatherStats (Weather 15 7.2)
-- temperature: 15.0
-- pressure: 7.2


------------------------

-- zoom :: Monad m=> Lens' s a -> StateT a m r -> StateT s m r

convertCelsiusToFahrenheit :: StateT Float IO ()
convertCelsiusToFahrenheit = do
  modify (\celsius -> (celsius * (9/5)) + 32)


weatherStats' :: StateT Weather IO ()
weatherStats' = do
  zoom temperature convertCelsiusToFahrenheit

-- >>> execStateT weatherStats (Weather 32 7.2)
-- Weather {_temperature = 89.6, _pressure = 7.2}
