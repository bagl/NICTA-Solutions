{-# LANGUAGE TypeFamilies #-}

module TicTacToe where

data Started
data InProgress
data Finished

data Move = Move Position (Player X')
data Board a = Board [Move]
data Position = Position Int Int
              deriving (Eq, Ord)

newtype PlayableBoard =
  PlayableBoard (Either (Board Started) (Board InProgress))
newtype NonEmptyBoard =
  NonEmptyBoard (Either (Board InProgress) (Board Finished))

move :: PlayableBoard
     -> Position
     -> NonEmptyBoard
move (PlayableBoard (Left b)) pos = NonEmptyBoard . Left $ firstMove b pos
move _ _ = error "move not implemented"

firstMove :: Board Started -> Position -> Board InProgress
firstMove _ pos = Board [Move pos firstPlayer]

type family NextPlayer a where
  NextPlayer X' = O'
  NextPlayer O' = X'

data X'
data O'

data Player a = Player

playerX :: Player X'
playerX = Player

playerO :: Player O'
playerO = Player

firstPlayer :: Player X'
firstPlayer = Player

nextPlayer :: Player a -> Player (NextPlayer a)
nextPlayer _ = Player

data Result = Winner (Player X')
            | Draw

data Field = FieldOccupied (Player X')
           | FieldEmpty

whoWon :: Board Finished -> Result
whoWon = undefined

playerAt :: Board a -> Position -> Field
playerAt (Board moves) position =
  maybe FieldEmpty FieldOccupied
  $ lookup position
  $ map (\(Move pos player) -> (pos, player)) moves
