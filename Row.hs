module Row (
  DataRow(DataRow),
  Datum(TextDatum, NumberDatum),
  rawGeneratedDataToRows
) where

import           Data.List.Split
import           Header


data DataRow = DataRow [Datum] deriving Show
data Datum = TextDatum String | NumberDatum Float deriving Show


rawGeneratedDataToRows :: [ColumnHeader] -> String -> [DataRow]
rawGeneratedDataToRows headers = map (parseDataLine headers) . drop 1 . lines

parseDataLine :: [ColumnHeader] -> String -> DataRow
parseDataLine headers = dataLineToRow (map headerType headers)

dataLineToRow :: [ColumnType] -> String -> DataRow
dataLineToRow headers = DataRow . zipWith parseLineItem headers . splitOn ","

parseLineItem :: ColumnType -> String -> Datum
parseLineItem NumberType = NumberDatum . (read :: String -> Float)
parseLineItem TextType = TextDatum
