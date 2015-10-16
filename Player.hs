module Player where

import Board

data Player = PX | PO

toCell :: Player -> Cell
toCell PX = X
toCell PO = O

move :: Player -> Int -> Board -> Board
move player pos = setCell pos (toCell player)

possibleBoards :: Player -> Board -> [Board]
possibleBoards player board = map (\pos -> move player pos board) posns
  where posns = filter (\ix -> getCell ix board == E) [0..8]

hasWon :: Player -> Board -> Bool
hasWon player board = or conditions
  where conditions :: [Bool]
        conditions =
          concatMap (\f ->
                       map (all (== c))
                           (f board))
                    [rows,cols,diags]
        c = toCell player
