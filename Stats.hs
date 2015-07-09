module Stats (
  columnCount,
  columnNullCount
) where

import           Column


columnCount :: DataColumn -> Int
columnCount (TextColumn _ items) = length items
columnCount (NumberColumn _ items) = length items

columnNullCount :: DataColumn -> Int
columnNullCount (TextColumn _ items) = numNothings items
columnNullCount (NumberColumn _ items) = numNothings items

numNothings :: [Maybe a] -> Int
numNothings = sum . map maybeToInt

maybeToInt :: Maybe a -> Int
maybeToInt Nothing = 1
maybeToInt (Just _) = 0
