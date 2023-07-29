{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Run (run) where

import Control.Monad.Freer hiding (run)
import Control.Monad.Freer.Fresh
import Control.Monad.Freer.Reader as FReader
import Control.Monad.Freer.Writer
import qualified Data.Map.Lazy as Map
import Import

borelDist :: StandardBorel t => Distribution t -> Distribution Borel
borelDist d = Distribution borelCdf borelPdf (borel . (quantile d)) where
  borelCdf :: Borel -> Double
  borelCdf b = fromMaybe 0 $ (cdf d) <$> deBorel b
  borelPdf :: Borel -> Double
  borelPdf b = fromMaybe 0 $ (pdf d) <$> deBorel b

runGenerative :: Members '[FReader.Reader HilbertCube, Writer Trace, Fresh] eff
                 => Eff (Generative ': eff) a -> Eff eff a
runGenerative = interpret (\case
  Sample a d -> do
    cube <- FReader.ask
    f <- fresh
    let val = (quantile d) $ cube (a, f) in do
      tell (Trace (Map.singleton (a, f) (borel val), []))
      return val
  Factor w -> tell (Trace (Map.empty, [w])))

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
