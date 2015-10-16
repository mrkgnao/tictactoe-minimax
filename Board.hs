module Board where

import Data.List (intercalate, unfoldr)

data Cell = O | X | E deriving (Read, Show, Eq)
data Board = Board [Cell] deriving Eq
type Index = Int

instance Show Board where
  show (Board cs) =
    "\n" ++ intercalate "\n" (map unwords [topRow,midRow,botRow]) ++ "\n"
    where lst = map show cs
          [topRow,midRow,botRow] = chunks 3 lst

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

emptyBoard :: Board
emptyBoard = Board (replicate 9 E)

invert :: Board -> Board
invert (Board cs) = Board $ map invert' cs
  where invert' X = O
        invert' O = X
        invert' E = E

-- FIXME use Maybe?
getCell :: Int -> Board -> Cell
getCell ix (Board cs)
  | 0 <= ix && ix <= 8 = cs !! ix
  | otherwise = error $ "Invalid index " ++ show ix

setCell :: Int -> Cell -> Board -> Board
setCell ix cell (Board cs) =
  Board $ cs // (ix,cell)
  where ls // (n,item) = a ++ (item : b)
          where (a,_:b) = splitAt n ls

rows :: Board -> [[Cell]]
rows (Board cs) = chunks 3 cs

cols :: Board -> [[Cell]]
cols (Board cs) = map (col cs) [0,1,2]
  where
    col :: [Cell] -> Int -> [Cell]
    col cs ix = map (\n -> cs !! (ix + 3 * n)) [0,1,2]

diags :: Board -> [[Cell]]
diags (Board cs) = [diag, antidiag]
  where diag = map (cs !!) [0,4,8]
        antidiag = map (cs !!) [2,4,7]
