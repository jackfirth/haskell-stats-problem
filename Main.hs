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
  let counts = map columnCount columns
  print ("Counts: " ++ show counts)
  let nullCounts = map columnNullCount columns
  print ("Null counts: " ++ show nullCounts)
  let numMins = mapOverNumColumns columnMin columns
  print ("Number column minimums: " ++ show numMins)
  let numMaxs = mapOverNumColumns columnMax columns
  print ("Number column maximums: " ++ show numMaxs)
  let numAverages = mapOverNumColumns columnAverage columns
  print ("Number column averages: " ++ show numAverages)

generateRaw :: Integer -> IO String
generateRaw n = readProcess "./generator" [show n] ""
