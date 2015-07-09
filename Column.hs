module Column (
  DataColumn,
  dataRowsToColumns
) where

import           Data.List
import           Header
import           Row

data DataColumn = TextColumn String [String] | NumberColumn String [Float] deriving Show

dataRowsToColumns :: [ColumnHeader] -> [DataRow] -> [DataColumn]
dataRowsToColumns headers rows = zipWith packDataIntoColumn headers (unpackDataRows headers rows)

unpackDataRows :: [ColumnHeader] -> [DataRow] -> [(ColumnType, [Datum])]
unpackDataRows headers rows = zipWith unpackDataRow headers (explodeRows rows)

unpackDataRow :: ColumnHeader -> [Datum] -> (ColumnType, [Datum])
unpackDataRow header dataList = (headerType header, dataList)

explodeRows :: [DataRow] -> [[Datum]]
explodeRows = transpose . map explodeRow

explodeRow :: DataRow -> [Datum]
explodeRow (DataRow dataList) = dataList

packDataIntoColumn :: ColumnHeader -> (ColumnType, [Datum]) -> DataColumn
packDataIntoColumn header (NumberType, dataList) = NumberColumn (headerName header) (numberData dataList)
packDataIntoColumn header (TextType, dataList) = TextColumn (headerName header) (textData dataList)

numberData :: [Datum] -> [Float]
numberData = map getNum where
  getNum :: Datum -> Float
  getNum (NumberDatum x) = x
  getNum _ = error "expected number data"

textData :: [Datum] -> [String]
textData = map getText where
  getText :: Datum -> String
  getText (TextDatum s) = s
  getText _ = error "expected text data"
