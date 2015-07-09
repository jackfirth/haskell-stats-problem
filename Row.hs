module Row (
  DataRow(DataRow),
  Datum(TextDatum, NumberDatum),
  rawGeneratedDataToRows
) where

import           Data.Char
import           Data.List.Split
import           Header


data DataRow = DataRow [Datum] deriving Show
data Datum = TextDatum (Maybe String) | NumberDatum (Maybe Float) deriving Show


rawGeneratedDataToRows :: [ColumnHeader] -> String -> [DataRow]
rawGeneratedDataToRows headers = map (parseDataLine headers) . drop 1 . lines

parseDataLine :: [ColumnHeader] -> String -> DataRow
parseDataLine headers = dataLineToRow (map headerType headers)

dataLineToRow :: [ColumnType] -> String -> DataRow
dataLineToRow headers = DataRow . zipWith parseLineItem headers . map filterWhitespace . splitOn ","

filterWhitespace :: String -> String
filterWhitespace = filter (not . isSpace)

parseLineItem :: ColumnType -> String -> Datum
parseLineItem NumberType "" = NumberDatum Nothing
parseLineItem NumberType item = NumberDatum (Just (read item))
parseLineItem TextType "" = TextDatum Nothing
parseLineItem TextType item = TextDatum (Just item)
