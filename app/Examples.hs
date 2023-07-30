module Examples (sidewalk, sidewalkProb, stdNPrior, substituteSidewalk) where

import Statistics.Distribution hiding (Distribution)
import Statistics.Distribution.Binomial
import Statistics.Distribution.Normal
import Run
import Types

stdN = Distribution (cumulative standard) (density standard)
                    (Statistics.Distribution.quantile standard)

discreteQuantile d u = quant 0 0 where
  quant k p = let p' = p + (probability d k) in
    if p <= u && u <= p' then
      k
    else
      quant (k+1) p'

bern p = binomial 1 p
bernoulli p = let b = bern p in
 Distribution (cumulative b . fromIntegral) (probability b) (discreteQuantile b)

stdNPrior = SampleE "z" (Lit stdN)

sidewalk :: Int -> Double -> Expr Int
sidewalk wet eps =
  Bind
    (SampleE "rain" (Lit $ bernoulli 0.8))
    (\rain -> let p = (if rain > 0 then 0.9 else eps) in
      Bind
        (FactorE (Appl (Lit $ pdf (bernoulli p)) (Lit $ wet)))
        (\() -> Lit rain)
    )

substituteSidewalk :: IO (WTrace, WTrace)
substituteSidewalk = do
  (rain1, WTrace trace1 w1) <- ancestor . eval $ model
  (rain2, WTrace trace2 w2) <- substitute trace1 . eval $ model
  putStrLn . show $ (rain1 == rain2)
  return (WTrace trace1 w1, WTrace trace2 w2) where
    model = sidewalk 1 0.001

sidewalkProb :: Bool -> IO (Int, WTrace)
sidewalkProb target = do
  (rain, WTrace trace w) <- ancestor . eval $ model
  (_, WTrace _ m) <- prob trace target . eval $ model
  return (rain, WTrace trace m) where
    model = sidewalk 1 0.001

-- def sidewalk_wet(wet, epsilon):
--     assert 0. < epsilon and epsilon < 1.
--     # Did it rain?
--     rain = sample("rain", Bernoulli(0.8))
--     # Is the sidewalk wet?
--     p = 0.9 if rain else epsilon
--     factor("wet", Bernoulli(p).pmf(wet))
--     return rain
