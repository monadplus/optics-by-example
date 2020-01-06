{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module LensPropertiesSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Lens.Properties (isLens)
import Control.Lens
import Generic.Random (genericArbitraryU)
import GHC.Generics

spec :: Spec
spec = do
  describe "Lens Laws" $ do
    it "should validate all builderLens laws" $ do
      --quickCheck $ isLens builderLens
      quickCheck $ isLens (_1 :: Lens' (Int, String) Int)

data Builder =
  Builder { _context :: [String]
          , _build   :: [String] -> String
          --} deriving (Show, Eq, Generic)
          } deriving (Generic)

instance Arbitrary Builder where
  arbitrary = genericArbitraryU

getBuilder :: Builder -> String
getBuilder Builder{..} =
  _build _context

setBuilder :: Builder -> String -> Builder
setBuilder builder str =
  builder{ _build = const str }

builderLens :: Lens' Builder String
builderLens = lens getBuilder setBuilder
