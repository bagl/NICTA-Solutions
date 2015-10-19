module TicTacToe
       ( emptyBoard
       , playerAt
       , firstMove
       , move
       , whoWon
       , takeBack
       , runGame
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

data Board =
  Board { boardShape :: BoardShape
        , nWins      :: Int
        , moves      :: [Move]
        } deriving (Eq, Show)

data NewBoard =
  NewBoard { boardShape' :: BoardShape
           , nWins'      :: Int
           } deriving (Show)

data FinishedBoard =
  FinishedBoard { boardShape'' :: BoardShape
                , nWins''      :: Int
                , moves''      :: [Move]
                } deriving (Show)

data NonEmptyBoard = NEInPlayBoard Board
                   | NEFinishedBoard FinishedBoard
                   deriving (Show)

data PlayableBoard = PNewBoard NewBoard
                   | PInPlayBoard Board

data AnyBoard = AEmpty NewBoard
              | ABoard Board
              | AFinished FinishedBoard
              deriving (Show)

data Position = Position Int Int
              deriving (Eq, Ord, Show)

data Move = Move Position Player
          deriving (Eq, Show)

data InvalidMoveErr = OutOfRangePosition
                    | AlreadyOccupiedField
                    | OtherPlayersTurn
                    deriving (Show)

data Outcome = InvalidMove InvalidMoveErr
             | InPlay Board
             | Finished FinishedBoard
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

data NonEmpty a = NonEmpty a [a]

emptyBoard :: Int -> Int -> Int -> NewBoard
emptyBoard nRows nCols n =
  NewBoard { boardShape' = BoardShape (nRows, nCols)
           , nWins'      = n
           }

firstMove :: NewBoard -> Position -> Board
firstMove board pos =
  Board { moves = [Move pos firstPlayer]
        , nWins = nWins' board
        , boardShape = boardShape' board }

move :: Board -> Position -> Outcome
move b pos =
  case playerAt aBoard pos of
    FieldOccupied _ -> InvalidMove AlreadyOccupiedField
    FieldEmpty -> case winningMove (ABoard newBoard) pos of
      NoWin   -> InPlay newBoard
      Win _ _ -> Finished (finish newBoard)
  where aBoard = ABoard b
        newMoves = Move pos (nextMovePlayer aBoard) : moves b
        newBoard = b { moves = newMoves }

runGame :: NewBoard -> NonEmpty Position -> Outcome
runGame b (NonEmpty p ps) =
  let b' = firstMove b p
  in case ps of
    []       -> InPlay b'
    (p':ps') -> runGame' (move b' p') ps'
  where
    runGame' :: Outcome -> [Position] -> Outcome
    runGame' err@InvalidMove{} _     = err
    runGame' fin@Finished{}    _     = fin
    runGame' out               []    = out
    runGame' (InPlay b'') (p'':ps'') =
      runGame' (move b'' p'') ps''

finish :: Board -> FinishedBoard
finish b = FinishedBoard { moves'' = moves b
                         , boardShape'' = boardShape b
                         , nWins'' = nWins b }

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

line :: AnyBoard -> Position -> Direction -> Maybe (ListZipper Position)
line b' pos dir = case b' of
  AEmpty _    -> Nothing
  ABoard b    -> f (boardShape b)
  AFinished b -> f (boardShape'' b)
  where
    f bs = let positions = linePositions bs pos dir
               l = playerAt b' <$> positions
           in case focus l of
                FieldEmpty      -> Nothing
                FieldOccupied _ ->
                  let sameLZ = sameAroundFocus l
                      nLeft  = length $ lefts sameLZ
                      nRight = length $ rights sameLZ
                  in Just $ takeLeft nLeft $ takeRight nRight positions

winningMove :: AnyBoard -> Position -> WinStats
winningMove board position =
  case (playerAt board position, winningLine) of
    (FieldOccupied p, Just l) -> Win p (toList l)
    _                         -> NoWin
  where
    pLines :: [ListZipper Position]
    pLines = catMaybes $ line board position <$> [Horizontal, Vertical, DiagonalBL, DiagonalUL]
    winningLine :: Maybe (ListZipper Position)
    winningLine = find ((nToWin board ==) . LZ.length) pLines
    nToWin (AEmpty b)    = nWins'  b
    nToWin (ABoard b)    = nWins   b
    nToWin (AFinished b) = nWins'' b

isValidPosition :: BoardShape -> Position -> Bool
isValidPosition (BoardShape (nRows, nCols)) (Position r c) =
  1 <= r && r <= nRows && 1 <= c && c <= nCols

nextMovePlayer :: AnyBoard -> Player
nextMovePlayer AEmpty{} = firstPlayer
nextMovePlayer board = case board of
  ABoard b    -> f $ moves b
  AFinished b -> f $ moves'' b
  where f []    = firstPlayer
        f (m:_) = nextPlayer $ player m

whoWon :: FinishedBoard -> Result
whoWon board = case moves'' board of
  (Move pos _:_) ->
    case winningMove (AFinished board) pos of
      NoWin   -> Draw
      Win p _ -> Winner p
  [] -> error "not possible: no moves on finished board"


playerAt :: AnyBoard -> Position -> Field
playerAt board' position = case board' of
  AEmpty{}    -> FieldEmpty
  ABoard b    -> f $ moves b
  AFinished b -> f $ moves'' b
  where f ms = maybe FieldEmpty FieldOccupied
               $ lookup position
               $ map (\(Move pos pla) -> (pos, pla)) ms

takeBack :: NonEmptyBoard -> PlayableBoard
takeBack board = case board of
  NEInPlayBoard b   -> takeBack' (moves b)   (boardShape b)   (nWins b)
  NEFinishedBoard b -> takeBack' (moves'' b) (boardShape'' b) (nWins'' b)
  where takeBack' ms bs nw =
          let newMoves = drop 1 ms
          in case newMoves of
               [] -> PNewBoard NewBoard { boardShape' = bs, nWins' = nw }
               _  -> PInPlayBoard Board { boardShape = bs, nWins = nw, moves = ms }
