import           Column
import           Header
import           Row
import           System.Process

main :: IO ()
main = do
  raw <- generateRaw 10
  let headers = rawGeneratedDataHeaders raw
  let rows = rawGeneratedDataToRows headers raw
  let columns = dataRowsToColumns headers rows
  print raw
  print columns

generateRaw :: Integer -> IO String
generateRaw n = readProcess "./generator" [show n] ""
