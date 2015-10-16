module Player where

import Board

data Player = PX | PO

toCell :: Player -> Cell
toCell PX = X
toCell PO = O

move :: Player -> Int -> Board -> Board
move player pos = setCell pos (toCell player)

-- | possibleBoards returns all board configurations that the current player can
-- | reach within one move.
possibleBoards :: Player -> Board -> [Board]
possibleBoards player board = map (\pos -> move player pos board) posns
  where posns = filter (\ix -> getCell ix board == E) [0..8]

-- | hasWon and hasLost will be very important for evaluating nodes of the game
-- | tree later. A player has won if at least one of the three rows, three
-- | and two diagonals "belongs" to them.
hasWon, hasLost :: Player -> Board -> Bool
hasWon player board = or conditions
  where conditions :: [Bool]
        conditions =
          concatMap (\f ->
                       map (all (== c))
                           (f board))
                    [rows,cols,diags]
        c = toCell player
-- Hehehe
hasLost player board = hasWon player (invert board)
