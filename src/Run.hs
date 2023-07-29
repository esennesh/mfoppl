{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Run (ancestor, eval, randomize, run) where

import Control.Monad.Freer hiding (run)
import Control.Monad.Freer.Fresh
import Control.Monad.Freer.Reader as FReader
import Control.Monad.Freer.Writer
import qualified Data.Map.Lazy as Map
import Import
import System.Random

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

eval :: Member Generative eff => Expr t -> Eff eff t
eval (Lit t) = pure t
eval (If c t e) = eval c >>= (\c -> if c then eval t else eval e)
eval (Appl f a) = do
  f <- eval f
  a <- eval a
  return (f a)
eval (Let a f) = do
  a <- eval a
  b <- eval (f a)
  return (a, b)
eval (SampleE a d) = (eval d) >>= sample a
eval (FactorE w)   = (eval w) >>= factor

seedCube :: RandomGen g => g -> HilbertCube
seedCube g (v, i) = ((us g) !! i) where
  us :: RandomGen g => g -> [Double]
  us g = (\(u, g') -> u:(us g')) $ random g

randomize :: (MonadIO m, LastMember m eff) =>
             Eff (FReader.Reader HilbertCube ': eff) t -> Eff eff t
randomize p = do
  g <- newStdGen
  FReader.runReader (seedCube g) p

ancestor :: MonadIO m => Eff '[Generative, FReader.Reader HilbertCube,
                               Writer Trace, Fresh, m] t ->
                         m (t, Trace)
ancestor = runM . evalFresh 0 . runWriter . randomize . runGenerative

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
