module Count (
  columnCount,
  columnNullCount
) where

import           Column


columnCount :: DataColumn -> Int
columnCount = callColumn length

columnNullCount :: DataColumn -> Int
columnNullCount = callColumn numNothings

numNothings :: [Maybe a] -> Int
numNothings = sum . map maybeToInt

maybeToInt :: Maybe a -> Int
maybeToInt Nothing = 1
maybeToInt (Just _) = 0
