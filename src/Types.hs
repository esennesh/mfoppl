{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Types
  ( Address
  , App (..)
  , Borel (..)
  , Distribution (..)
  , Expr (..)
  , factor
  , Generative (..)
  , HilbertCube
  , WTrace (..)
  , Options (..)
  , sample
  , StandardBorel (..)
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
type HilbertCube = Int -> Double
type Trace = Map.Map Address Borel
data WTrace = WTrace Trace Double deriving (Eq, Show)

instance Semigroup WTrace where
  (WTrace ta wa) <> (WTrace tb wb) = WTrace (Map.union ta tb) (wa * wb)

instance Monoid WTrace where
  mempty = WTrace Map.empty 1

data Expr t where
  Lit     :: t -> Expr t
  If      :: Expr Bool -> Expr a -> Expr a -> Expr a
  Appl    :: Expr (a -> b) -> Expr a -> Expr b
  Bind    :: Expr t -> (t -> Expr u) -> Expr u
  SampleE :: StandardBorel t => String -> Expr (Distribution t) -> Expr t
  FactorE :: Expr Double -> Expr ()
