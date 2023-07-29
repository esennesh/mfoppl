{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Types
  ( Address
  , App (..)
  , Borel (..)
  , Distribution (..)
  , Generative (..)
  , HilbertCube
  , Options (..)
  , StandardBorel (..)
  , Trace (..)
  ) where

import Control.Monad.Freer.TH
import qualified Data.Map.Lazy as Map
import RIO
import RIO.Process

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

data Borel = Unit | Integer Int | Real Double | Product Borel Borel
             deriving (Eq, Show)
class StandardBorel t where
  borel :: t -> Borel
  deBorel :: Borel -> Maybe t
instance StandardBorel () where
  borel () = Unit
  deBorel Unit = Just ()
  deBorel _    = Nothing
instance StandardBorel Int where
  borel = Integer
  deBorel (Integer i) = Just i
  deBorel _           = Nothing
instance StandardBorel Double where
  borel = Real
  deBorel (Real r) = Just r
  deBorel _        = Nothing
instance (StandardBorel a, StandardBorel b) => StandardBorel (a, b) where
  borel (a, b) = Product (borel a) (borel b)
  deBorel (Product a b) = do
    ta <- deBorel a
    tb <- deBorel b
    return (ta, tb)
  deBorel _             = Nothing

data Distribution t = Distribution
  { cdf :: t -> Double
  , pdf :: t -> Double
  , quantile :: Double -> t
  }

data Generative r where
  Sample :: StandardBorel t => String -> Distribution t -> Generative t
  Factor :: Double -> Generative ()
makeEffect ''Generative

type Address = (String, Int)
type HilbertCube = Address -> Double
newtype Trace = Trace (Map.Map Address Borel, [Double]) deriving (Eq, Show)

instance Semigroup Trace where
  (Trace (da, la)) <> (Trace (db, lb)) = Trace (Map.union da db, la ++ lb)

instance Monoid Trace where
  mempty = Trace (Map.empty, [])
