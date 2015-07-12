module Average (
  average
) where


average :: [Float] -> Float
average xs = sum xs / fromIntegral (length xs)
