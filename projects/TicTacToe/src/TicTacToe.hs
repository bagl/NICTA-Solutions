module TicTacToe
       ( emptyBoard
       , playerAt
       , move
       , whoWon
       , takeBack
       , Result(..)
       , InvalidMoveErr(..)
       , Player(..)
       ) where

import           Data.List (find)
import           Data.Maybe (catMaybes)
import           ListZipper hiding (length)
import qualified ListZipper as LZ

data Player = X | O
            deriving (Eq, Show)

data BEmpty
data BInPlay
data BFinished

data Board a =
  Board { boardShape :: BoardShape
        , nWins      :: Int
        , moves      :: [Move]
        } deriving (Eq, Show)

type NewBoard = Board BEmpty
type InPlayBoard = Board BInPlay
type FinishedBoard = Board BFinished

data PlayableBoard = PNewBoard NewBoard
                   | PInPlayBoard InPlayBoard
                   deriving (Show)

data NonEmptyBoard = NEInPlayBoard InPlayBoard
                   | NEFinishedBoard FinishedBoard
                   deriving (Show)

data Position = Position Int Int
              deriving (Eq, Ord, Show)

data Move = Move Position Player
          deriving (Eq, Show)

data InvalidMoveErr = OutOfRangePosition
                    | AlreadyOccupiedField
                    | OtherPlayersTurn
                    deriving (Show)

newtype BoardShape = BoardShape (Int, Int)
                   deriving (Eq, Show)

data Result = Winner Player
            | Draw

data Field = FieldOccupied Player
           | FieldEmpty
           deriving (Eq, Show)

data WinStats = NoWin | Win Player [Position]
              deriving (Show)

data Direction
  = Horizontal
  | Vertical
  | DiagonalBL
  | DiagonalUL

emptyBoard :: Int -> Int -> Int -> PlayableBoard
emptyBoard nRows nCols n = PNewBoard
  Board { boardShape = BoardShape (nRows, nCols)
        , nWins      = n
        , moves      = []
        }

move :: PlayableBoard
     -> Position
     -> Either InvalidMoveErr NonEmptyBoard
move (PNewBoard b)    pos =
  Right $ NEInPlayBoard $ firstMove b pos
move (PInPlayBoard b) pos =
  case playerAt b pos of
    FieldOccupied _ -> Left AlreadyOccupiedField
    FieldEmpty -> case winningMove newBoard pos of
      NoWin   -> Right $ NEInPlayBoard newBoard
      Win _ _ -> Right $ NEFinishedBoard newBoard
  where newBoard = b { moves = Move pos (nextMovePlayer b) : moves b }

firstMove :: NewBoard -> Position -> InPlayBoard
firstMove board pos = board { moves = [Move pos firstPlayer] }

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

player :: Move -> Player
player (Move _ p) = p

firstPlayer :: Player
firstPlayer = X

linePositions :: BoardShape -> Position -> Direction -> ListZipper Position
linePositions bShape pos dir = ListZipper (l (-)) pos (l (+))
  where l f = takeWhile (isValidPosition bShape) $ drop 1 $ iterate (doDelta f) pos
        doDelta f (Position x y) = Position (f x dx) (f y dy)
        (dx, dy) = case dir of
          Horizontal -> (0,  1)
          Vertical   -> (1,  0)
          DiagonalBL -> (1, -1)
          DiagonalUL -> (1,  1)

line :: Board a -> Position -> Direction -> Maybe (ListZipper Position)
line board pos dir =
  case focus l of
    FieldEmpty      -> Nothing
    FieldOccupied _ ->
      let sameLZ = sameAroundFocus l
          nLeft  = length $ lefts sameLZ
          nRight = length $ rights sameLZ
      in Just $ takeLeft nLeft $ takeRight nRight positions
  where positions = linePositions (boardShape board) pos dir
        l = playerAt board <$> positions

winningMove :: Board a -> Position -> WinStats
winningMove board position =
  case (playerAt board position, winningLine) of
    (FieldOccupied p, Just l) -> Win p (toList l)
    _                         -> NoWin
  where
    pLines :: [ListZipper Position]
    pLines = catMaybes $ line board position <$> [Horizontal, Vertical, DiagonalBL, DiagonalUL]
    winningLine :: Maybe (ListZipper Position)
    winningLine = find ((nWins board ==) . LZ.length) pLines

isValidPosition :: BoardShape -> Position -> Bool
isValidPosition (BoardShape (nRows, nCols)) (Position r c) =
  1 <= r && r <= nRows && 1 <= c && c <= nCols

nextMovePlayer :: Board a -> Player
nextMovePlayer board = case moves board of
  []    -> firstPlayer
  (m:_) -> nextPlayer $ player m

whoWon :: FinishedBoard -> Result
whoWon board = case moves board of
  (Move pos _:_) ->
    case winningMove board pos of
      NoWin   -> Draw
      Win p _ -> Winner p
  [] -> error "not possible: no moves on finished board"


playerAt :: Board a -> Position -> Field
playerAt board position =
  maybe FieldEmpty FieldOccupied
  $ lookup position
  $ map (\(Move pos pla) -> (pos, pla)) (moves board)

takeBack :: NonEmptyBoard -> PlayableBoard
takeBack board = case board of
  NEInPlayBoard b   -> takeBack' b
  NEFinishedBoard b -> takeBack' b
  where takeBack' b' =
          let newMoves = drop 1 (moves b')
              newBoard = b' { moves = newMoves }
          in case newMoves of
          [] -> PNewBoard newBoard
          _  -> PInPlayBoard newBoard
