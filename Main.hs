import           Column
import           Count
import           DistinctCount
import           Header
import           NumStats
import           Row
import           System.Process
import           TextStats


data Stats = Stats {
  -- counts         :: [Int],
  nullCounts :: [Int],
  minNums    :: [Maybe Float],
  maxNums    :: [Maybe Float]
  -- averages       :: [Maybe Float],
  -- shortestCounts :: [ShortestCount],
  -- longestCounts  :: [LongestCount],
  -- averageLengths :: [Float]
} deriving (Show)

calcStats :: [DataColumn] -> Stats
calcStats columns = Stats {
  -- counts = map columnCount columns,
  nullCounts = map columnNullCount columns,
  minNums = mapOverNumColumns columnMin columns,
  maxNums = mapOverNumColumns columnMax columns
  -- averages = mapOverNumColumns columnAverage columns,
  -- shortestCounts = mapOverTextColumns columnShortestCount columns,
  -- longestCounts = mapOverTextColumns columnLongestCount columns,
  -- averageLengths = mapOverTextColumns columnAverageLength columns
}

pullCount :: Integer
pullCount = 10000

rowsCount :: Integer
rowsCount = 100000

main :: IO ()
main = do
  columns <- generateDataColumns rowsCount
  print (calcStats columns)
  -- printColumnsStat (mapOverTextColumns columnDistinctCounts) "Text column distinct items count" columns

generateDataColumns :: Integer -> IO [DataColumn]
generateDataColumns n = do
  raw <- generateRaw 0
  let headers = rawGeneratedDataHeaders raw
  rows <- generateDataRows headers n
  return (dataRowsToColumns headers rows)

generateDataRows :: [ColumnHeader] -> Integer -> IO [DataRow]
generateDataRows headers n
  | n <= pullCount = do
      raw <- generateRaw pullCount
      return (rawGeneratedDataToRows headers raw)
  | otherwise = do
      nextBatch <- generateDataRows headers pullCount
      rest <- generateDataRows headers (n - pullCount)
      return (nextBatch ++ rest)

generateRaw :: Integer -> IO String
generateRaw n = readProcess "./generator" [show n] ""
