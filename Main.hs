import           Header
import           Row
import           System.Process

main :: IO ()
main = do
  raw <- generateRaw 3
  let headers = rawGeneratedDataHeaders raw
  let rows = rawGeneratedDataToRows headers raw
  print headers
  print rows


generateRaw :: Integer -> IO String
generateRaw n = readProcess "./generator" [show n] ""
