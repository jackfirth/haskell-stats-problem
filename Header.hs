module Header (
  ColumnType,
  ColumnHeader,
  rawGeneratedDataHeaders
) where

import           Data.Char
import           Data.List.Split


data ColumnType = Number | Text deriving Show
data ColumnHeader = ColumnHeader String ColumnType deriving Show

rawGeneratedDataHeaders :: String -> [ColumnHeader]
rawGeneratedDataHeaders = map parseHeader . splitRawColumnInfo . firstLine


firstLine :: String -> String
firstLine = head . lines

splitRawColumnInfo :: String -> [String]
splitRawColumnInfo = map removeQuotes . splitOn ","

parseHeader :: String -> ColumnHeader
parseHeader = makeHeader . stringRecordToNameAndType


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
