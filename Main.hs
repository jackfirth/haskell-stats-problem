import           Data.Char
import           Data.List.Split
import           System.Process

main :: IO ()
main = do
  raw <- generateRaw 3
  print (rawGeneratedDataHeader raw)


generateRaw :: Integer -> IO String
generateRaw n = readProcess "./generator" [show n] ""

data ColumnType = Number | Text deriving Show
data ColumnHeader = ColumnHeader String ColumnType deriving Show

rawGeneratedDataHeader :: String -> [ColumnHeader]
rawGeneratedDataHeader = map parseHeader . splitRawColumnInfo . firstLine


firstLine :: String -> String
firstLine = head . lines

splitRawColumnInfo :: String -> [String]
splitRawColumnInfo = splitOn ","

parseHeader :: String -> ColumnHeader
parseHeader = makeHeader . stringRecordToNameAndType . removeQuotes


removeQuotes :: String -> String
removeQuotes = filter (/= '"')

stringRecordToNameAndType :: String -> (String, String)
stringRecordToNameAndType s = let (name:typeName:_) = splitOnOpenParen s in
  (name, typeName)

splitOnOpenParen :: String -> [String]
splitOnOpenParen = map filterWhitespace . splitOn "(" . filter (/= ')')

filterWhitespace :: String -> String
filterWhitespace = filter (not . isSpace)

makeHeader :: (String, String) -> ColumnHeader
makeHeader (name, "number") = ColumnHeader name Number
makeHeader (name, "text") = ColumnHeader name Text
makeHeader (name, unknown) = error ("Unknown column type " ++ unknown ++ " in column " ++ name)
