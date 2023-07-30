{-# LANGUAGE FlexibleContexts #-}
module Examples (sidewalk, sidewalkDensity, sidewalkE, sidewalkProbE, stdNPrior,
                 stdNProb, substituteSidewalkE) where

import Control.Monad.Freer
import Statistics.Distribution hiding (Distribution)
import Statistics.Distribution.Binomial
import Statistics.Distribution.Normal
import Run
import Types

stdN :: Distribution Double
stdN = Distribution (cumulative standard) (density standard)
                    (Statistics.Distribution.quantile standard)

discreteQuantile :: DiscreteDistr d => d -> Double -> Int
discreteQuantile d u = quant 0 0 where
  quant k p = let p' = p + (probability d k) in
    if p <= u && u <= p' then
      k
    else
      quant (k+1) p'

bern :: Double -> BinomialDistribution
bern p = binomial 1 p

bernoulli :: Double -> Distribution Int
bernoulli p = let b = bern p in
 Distribution (cumulative b . fromIntegral) (probability b) (discreteQuantile b)

stdNPrior :: Expr Double
stdNPrior = SampleE "z" (Lit stdN)

stdNProb :: IO (Double, WTrace)
stdNProb = do
  (z, WTrace trace _) <- simulate . eval $ stdNPrior
  (_, WTrace _ m) <- traceDensity trace True . eval $ stdNPrior
  return (z, WTrace trace m)

sidewalkE :: Int -> Double -> Expr Int
sidewalkE wet eps =
  Bind
    (SampleE "rain" (Lit $ bernoulli 0.8))
    (\rain -> let p = (if rain > 0 then 0.9 else eps) in
      Bind
        (FactorE (Appl (Lit $ pdf (bernoulli p)) (Lit $ wet)))
        (\() -> Lit rain)
    )

sidewalk :: Member Generative eff => Int -> Double -> Eff eff Int
sidewalk wet eps = do
  rain <- sample "rain" (bernoulli 0.8)
  let pWet = if rain > 0 then 0.9 else eps in do
    observe (bernoulli pWet) wet

sidewalkDensity :: Bool -> IO (Int, WTrace)
sidewalkDensity target = do
  (rain, WTrace trace _) <- simulate model
  (_, WTrace _ m) <- traceDensity trace target model
  return (rain, WTrace trace m) where
    model = sidewalk 1 0.001

substituteSidewalkE :: IO (WTrace, WTrace)
substituteSidewalkE = do
  (rain1, WTrace trace1 w1) <- simulate . eval $ model
  (rain2, WTrace trace2 w2) <- replay trace1 . eval $ model
  putStrLn . show $ (rain1 == rain2)
  return (WTrace trace1 w1, WTrace trace2 w2) where
    model = sidewalkE 1 0.001

sidewalkProbE :: Bool -> IO (Int, WTrace)
sidewalkProbE target = do
  (rain, WTrace trace _) <- simulate . eval $ model
  (_, WTrace _ m) <- traceDensity trace target . eval $ model
  return (rain, WTrace trace m) where
    model = sidewalkE 1 0.001
