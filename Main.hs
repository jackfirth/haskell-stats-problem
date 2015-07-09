import           Column
import           Header
import           Row
import           Stats
import           System.Process

main :: IO ()
main = do
  raw <- generateRaw 100
  let headers = rawGeneratedDataHeaders raw
  let rows = rawGeneratedDataToRows headers raw
  let columns = dataRowsToColumns headers rows
  let counts = map columnCount columns
  let nullCounts = map columnNullCount columns
  print "Counts: "
  print counts
  print "Null counts: "
  print nullCounts

generateRaw :: Integer -> IO String
generateRaw n = readProcess "./generator" [show n] ""
