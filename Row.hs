module Row (
  DataRow,
  Datum,
  rawGeneratedDataToRows
) where

import           Data.List.Split
import           Header


type DataRow = [Datum]
data Datum = TextDatum String | NumberDatum Float deriving Show


rawGeneratedDataToRows :: [ColumnHeader] -> String -> [DataRow]
rawGeneratedDataToRows headers = map (dataLineToRow (map headerType headers)) . drop 1 . lines

dataLineToRow :: [ColumnType] -> String -> DataRow
dataLineToRow headers line = zipWith parseLineItem headers (splitOn "," line)

parseLineItem :: ColumnType -> String -> Datum
parseLineItem NumberColumn = NumberDatum . read
parseLineItem TextColumn = TextDatum
