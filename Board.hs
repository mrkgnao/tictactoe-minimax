module Board where

import Data.Array
import Data.List (intercalate, unfoldr)

data Cell = O | X | E deriving (Read, Show, Eq)
data Board = Board (Array (Int, Int) Cell) deriving Eq

instance Show Board where
  show (Board arr) =
    intercalate "\n" (map unwords [topRow,midRow,botRow])
    where lst = map show $ elems arr
          [topRow,midRow,botRow] = chunks 3 lst
          chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)
