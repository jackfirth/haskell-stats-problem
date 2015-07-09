import           Data.List.Split
import           System.Process

main :: IO ()
main = do
  raw <- generateRaw 3
  print (rawGeneratedDataHeader raw)


generateRaw :: Integer -> IO String
generateRaw n = readProcess "./generator" [show n] ""

data ColumnHeader = ColumnHeader String String deriving Show

rawGeneratedDataHeader :: String -> [ColumnHeader]
rawGeneratedDataHeader = map parseHeader . splitRawColumnInfo . firstLine


firstLine :: String -> String
firstLine = head . lines

splitRawColumnInfo :: String -> [String]
splitRawColumnInfo = splitOn ","

parseHeader :: String -> ColumnHeader
parseHeader _ = error "not implemented"
