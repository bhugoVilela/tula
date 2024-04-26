import Control.Monad (replicateM)
import Data.List (transpose)

generateCombinations :: [a] -> [b] -> [[(a, b)]]
generateCombinations names values =
  let valueCombinations = replicateM (length names) values
      nameValueCombinations = map (zip names) valueCombinations
   in nameValueCombinations
