{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Run
  ( eval
  , prob
  , replay
  , run
  , runRandomized
  , runRandomizedM
  , runVariates
  , simulate
  ) where

import Control.Monad.Freer hiding (run)
import Control.Monad.Freer.Fresh
import Control.Monad.Freer.Reader as FReader
import Control.Monad.Freer.Writer
import qualified Data.Map.Lazy as Map
import Import

runVariates :: (MonadIO m, LastMember m eff) => Trace ->
               Eff (FReader.Reader Variates ': eff) t -> Eff eff t
runVariates trace p = do
  cube <- hilbertCube
  FReader.runReader (conds, cube) p where
    conds = Map.mapKeys (\(a, f) -> a) trace

runRandomized :: Members '[FReader.Reader Variates, Fresh] eff =>
                 Eff (Randomized ': eff) a -> Eff eff a
runRandomized = interpret (\case
  Variate addr quant -> do
    (subst, cube) <- FReader.ask
    pc <- fresh
    case query addr subst of
      Just val -> return (pc, True, val)
      Nothing -> let val = quant (cube !! pc) in return (pc, False, val))
  where
    query :: StandardBorel t => String -> Map.Map String Borel -> Maybe t
    query a subst = Map.lookup a subst >>= deBorel

runGenerative :: Members '[Randomized, Writer WTrace] eff =>
                 Eff (Generative ': eff) a -> Eff eff a
runGenerative = interpret (\case
  Sample a d -> do
    (pc, _, val) <- variate a (quantile d)
    tell (WTrace (Map.singleton (a, pc) (borel val)) 1)
    return val
  Factor w -> tell (WTrace Map.empty w)) where
    query :: StandardBorel t => String -> Map.Map String Borel -> Maybe t
    query a subst = do
      borel <- Map.lookup a subst
      deBorel borel

runDensity :: Members '[Randomized, Writer WTrace] eff => Bool ->
              Eff (Generative ': eff) a -> Eff eff a
runDensity target = interpret (\case
  Sample a d -> do
    (pc, substituted, val) <- variate a (quantile d)
    let likelihood = if substituted then (pdf d val) else 0 in do
      tell (WTrace (Map.singleton (a, pc) (borel val)) likelihood)
      return val
  Factor w -> if target then tell (WTrace Map.empty w) else pure ())

eval :: Member Generative eff => Expr t -> Eff eff t
eval (Lit t) = pure t
eval (If c t e) = eval c >>= (\c -> if c then eval t else eval e)
eval (Appl f a) = do
  f <- eval f
  a <- eval a
  return (f a)
eval (Bind a f) = do
  a <- eval a
  b <- eval (f a)
  return b
eval (SampleE a d) = (eval d) >>= sample a
eval (FactorE w)   = (eval w) >>= factor

runRandomizedM :: MonadIO m => Trace ->
                               Eff '[Randomized, FReader.Reader Variates,
                                     Writer WTrace, Fresh, m] t ->
                               m (t, WTrace)
runRandomizedM trace =
  runM . evalFresh 0 . runWriter . runVariates trace . runRandomized

simulate :: MonadIO m => Eff '[Generative, Randomized, FReader.Reader Variates,
                               Writer WTrace, Fresh, m] t ->
                         m (t, WTrace)
simulate = replay Map.empty

replay :: MonadIO m => Trace ->
                       Eff '[Generative, Randomized, FReader.Reader Variates,
                             Writer WTrace, Fresh, m] t ->
                       m (t, WTrace)
replay trace = runRandomizedM trace . runGenerative

prob :: MonadIO m => Trace -> Bool ->
                     Eff '[Generative, Randomized, FReader.Reader Variates,
                           Writer WTrace, Fresh, m] t ->
                     m (t, WTrace)
prob trace target = runRandomizedM trace . runDensity target

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
