{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Run (ancestor, eval, prob, randomize, run, substitute, traceRandom) where

import Control.Monad.Freer hiding (run)
import Control.Monad.Freer.Fresh
import Control.Monad.Freer.Reader as FReader
import Control.Monad.Freer.Writer
import qualified Data.Map.Lazy as Map
import Import
import System.Random

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

runGenerative :: Members '[FReader.Reader Variates, Writer WTrace, Fresh] eff
                 => Eff (Generative ': eff) a -> Eff eff a
runGenerative = interpret (\case
  Sample a d -> do
    (subst, cube) <- FReader.ask
    pc <- fresh
    let val = fromMaybe (quantile d $ cube pc) (query a subst) in do
      tell (WTrace (Map.singleton (a, pc) (borel val)) 1)
      return val
  Factor w -> tell (WTrace Map.empty w)) where
    query :: StandardBorel t => String -> Map.Map String Borel -> Maybe t
    query a subst = do
      borel <- Map.lookup a subst
      deBorel borel

runDensity :: Members '[FReader.Reader Variates, Writer WTrace, Fresh] eff =>
              Bool -> Eff (Generative ': eff) a -> Eff eff a
runDensity target = interpret (\case
  Sample a d -> do
    (subst, cube) <- FReader.ask
    pc <- fresh
    case (Map.lookup a subst) >>= deBorel of
      Just val -> do
        tell (WTrace (Map.singleton (a, pc) (borel val)) (pdf d val))
        return val
      Nothing -> let val = quantile d $ cube pc in do
        tell (WTrace (Map.singleton (a, pc) (borel val)) 0)
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

seedCube :: RandomGen g => g -> HilbertCube
seedCube g i = ((us g) !! i) where
  us :: RandomGen g => g -> [Double]
  us = (\(u, g') -> u:(us g')) . random

randomize :: (MonadIO m, LastMember m eff) => Trace ->
             Eff (FReader.Reader Variates ': eff) t -> Eff eff t
randomize trace p = do
  g <- newStdGen
  FReader.runReader (conds, seedCube g) p where
    conds = Map.mapKeys (\(a, f) -> a) trace

traceRandom :: MonadIO m => Trace ->
                            Eff '[FReader.Reader Variates,
                                  Writer WTrace, Fresh, m] t ->
                            m (t, WTrace)
traceRandom trace = runM . evalFresh 0 . runWriter . randomize trace

ancestor :: MonadIO m => Eff '[Generative, FReader.Reader Variates,
                               Writer WTrace, Fresh, m] t ->
                         m (t, WTrace)
ancestor = substitute Map.empty

substitute :: MonadIO m => Trace -> Eff '[Generative, FReader.Reader Variates,
                                          Writer WTrace, Fresh, m] t ->
                           m (t, WTrace)
substitute trace = traceRandom trace . runGenerative

prob :: MonadIO m => Trace -> Bool ->
                        Eff '[Generative, FReader.Reader Variates,
                              Writer WTrace, Fresh, m] t ->
                        m (t, WTrace)
prob trace target = traceRandom trace . runDensity target

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
