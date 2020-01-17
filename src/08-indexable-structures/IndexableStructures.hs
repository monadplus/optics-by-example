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
{-# LANGUAGE LambdaCase          #-}
module IndexableStructures where

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

foo
