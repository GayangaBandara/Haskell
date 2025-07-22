-- Newton-Raphson successive square root approximations
allApproximations :: Double -> Double -> [Double]
allApproximations a0 n = iterate next a0
  where
    next a = (a + n / a) / 2
