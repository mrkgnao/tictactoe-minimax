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

-- | Found this on SO. Isn't it beautiful?
chunks :: Index -> [a] -> [[a]]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

-- | The "zero board"
emptyBoard :: Board
emptyBoard = Board (replicate 9 E)

-- | Convenience function that turns Xs into Os and vice versa.
invert :: Board -> Board
invert (Board cs) = Board $ map invert' cs
  where invert' X = O
        invert' O = X
        invert' E = E

-- | Not using Maybe because this will not be exposed.
getCell :: Index -> Board -> Cell
getCell ix (Board cs)
  | 0 <= ix && ix <= 8 = cs !! ix
  | otherwise = error $ "Invalid index " ++ show ix

-- | Set a cell of a board to a particular value.
-- | FIXME fail if user attempts to update non-empty cell?
setCell :: Index -> Cell -> Board -> Board
setCell ix cell (Board cs) =
  Board $ cs // (ix,cell)
  where ls // (n,item) = a ++ (item : b)
          where (a,_:b) = splitAt n ls

-- | Functions for working with the three kinds of subsets of a board
-- | that a player can "win": rows, columns and diagonals.
rows, cols, diags :: Board -> [[Cell]]
rows (Board cs) = chunks 3 cs
cols (Board cs) =
  map (col cs)
      [0,1,2]
  where col :: [Cell] -> Index -> [Cell]
        col cs ix =
          map (\n -> cs !! (ix + 3 * n))
              [0,1,2]
diags (Board cs) = [diag,antidiag]
  where diag = map (cs !!) [0,4,8]
        antidiag = map (cs !!) [2,4,7]
