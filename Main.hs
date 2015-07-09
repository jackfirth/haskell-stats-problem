import           Header
import           System.Process

main :: IO ()
main = do
  raw <- generateRaw 3
  print (rawGeneratedDataHeaders raw)


generateRaw :: Integer -> IO String
generateRaw n = readProcess "./generator" [show n] ""
