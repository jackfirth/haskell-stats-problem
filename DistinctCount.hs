module DistinctCount (
  columnDistinctCounts
) where

import           Column
import           Data.HashMap.Strict as HM
import           Data.List           (foldl')
import           Data.Maybe


type DistinctCounts = HashMap String Int

columnDistinctCounts :: DataColumn -> DistinctCounts
columnDistinctCounts = callTextColumn (distinctCounts . catMaybes)

distinctCounts :: [String] -> DistinctCounts
distinctCounts = Data.List.foldl' updateDistinctCount HM.empty

updateDistinctCount :: DistinctCounts -> String -> DistinctCounts
updateDistinctCount counts s = case HM.lookup s counts of
  Just count -> insert s (count + 1) counts
  Nothing -> insert s 1 counts
