import           Column
import           Count
import           Header
import           NumStats
import           Row
import           System.Process
import           TextStats

main :: IO ()
main = do
  columns <- generateDataColumns 10000
  printColumnsStat (map columnCount) "Counts" columns
  printColumnsStat (map columnNullCount) "Null counts" columns
  printColumnsStat (mapOverNumColumns columnMin) "Number column minimums" columns
  printColumnsStat (mapOverNumColumns columnMax) "Number column maximums" columns
  printColumnsStat (mapOverNumColumns columnAverage) "Number column averages" columns
  printColumnsStat (mapOverTextColumns columnShortestCount) "Text column shortest item count" columns

generateDataColumns :: Integer -> IO [DataColumn]
generateDataColumns n = do
  raw <- generateRaw n
  let headers = rawGeneratedDataHeaders raw
  let rows = rawGeneratedDataToRows headers raw
  return (dataRowsToColumns headers rows)

printColumnsStat :: Show a => ([DataColumn] -> a) -> String -> [DataColumn] -> IO ()
printColumnsStat f description columns = print (description ++ ": " ++ show (f columns))

generateRaw :: Integer -> IO String
generateRaw n = readProcess "./generator" [show n] ""
