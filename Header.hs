module Header (
  ColumnType(NumberColumn, TextColumn),
  ColumnHeader,
  headerType,
  rawGeneratedDataHeaders
) where

import           Data.Char
import           Data.List.Split


data ColumnType = NumberColumn | TextColumn deriving Show
data ColumnHeader = ColumnHeader String ColumnType deriving Show

headerType :: ColumnHeader -> ColumnType
headerType (ColumnHeader _ typeName) = typeName

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
makeHeader (name, "number") = ColumnHeader name NumberColumn
makeHeader (name, "text") = ColumnHeader name TextColumn
makeHeader (name, unknown) = error ("Unknown column type " ++ unknown ++ " in column " ++ name)
