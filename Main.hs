import           Column
import           Count
import           Header
import           NumStats
import           Row
import           System.Process

main :: IO ()
main = do
  raw <- generateRaw 10000
  let headers = rawGeneratedDataHeaders raw
  let rows = rawGeneratedDataToRows headers raw
  let columns = dataRowsToColumns headers rows
  printColumnsStat (map columnCount) "Counts" columns
  printColumnsStat (map columnNullCount) "Null counts" columns
  printColumnsStat (mapOverNumColumns columnMin) "Number column minimums" columns
  printColumnsStat (mapOverNumColumns columnMax) "Number column maximums" columns
  printColumnsStat (mapOverNumColumns columnAverage) "Number column averages" columns

printColumnsStat :: Show a => ([DataColumn] -> a) -> String -> [DataColumn] -> IO ()
printColumnsStat f description columns = print (description ++ ": " ++ show (f columns))

generateRaw :: Integer -> IO String
generateRaw n = readProcess "./generator" [show n] ""
