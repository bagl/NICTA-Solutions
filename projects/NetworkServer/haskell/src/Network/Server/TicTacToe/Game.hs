module Network.Server.TicTacToe.Game where

import Data.TicTacToe
import Network.Server.Common.Env
import Network.Server.Common.HandleLens
import Network.Server.Common.Lens
import Network.Server.Common.Line
import Network.Server.Common.Ref
import Network.Server.TicTacToe.Loop
import Data.Char(isSpace, toLower, toUpper)
import Data.Function(on)
import Data.IORef(readIORef, atomicModifyIORef)
import Data.Map (singleton)
import Data.Maybe(fromMaybe)
import Data.Foldable(msum, find)
import Data.Set(Set)
import Control.Applicative((<$), (<$>))
import System.IO(hGetLine, hPutStrLn)

type FinishedGames = [FinishedBoard]

type Game a = IORefLoop Board (Board, FinishedGames) a

data Command = Move Position
             | Current
             | Finished
             | Chat String
             | Turn
             | At Position
             | Unknown String
             deriving (Eq, Show)

-- |
--
-- >>> command "MOVE ne"
-- Move NE
--
-- >>> command "MOVE 2"
-- Move N
--
-- >>> command "GAME"
-- Current
--
-- >>> command "FiniSHED"
-- Finished
--
-- >>> command "CHAT hi"
-- Chat "hi"
--
-- >>> command "Turn"
-- Turn
--
-- >>> command "At 4"
-- At W
--
-- >>> command "At C"
-- At C
--
-- >>> command "At X"
-- Unknown "At X"
--
-- >>> command "Move i"
-- Unknown "Move i"

command :: String -> Command
command z =
  let p l = trimSpaces <$> prefixThen ((==) `on` toLower) l z
  in Unknown z `fromMaybe` msum [ Move     <$> (p "MOVE" >>= sPosition)
                                , Current  <$   p "GAME"
                                , Finished <$   p "FINISHED"
                                , Chat     <$>  p "CHAT"
                                , Turn     <$   p "TURN"
                                , At       <$> (p "AT" >>= sPosition) ]

trimSpaces :: String -> String
trimSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- |
--
-- >>> sPosition "1"
-- Just NW
--
-- > sPosition "E"
-- Just E
--
-- > sPosition "sw"
-- Just SW
--
-- > sPosition "x"
-- Nothing
sPosition :: String -> Maybe Position
sPosition s =
  let table = [ (["1", "NW"], NW)
              , (["2", "N"] , N )
              , (["3", "NE"], NE)
              , (["4", "W"] , W )
              , (["5", "C"] , C )
              , (["6", "E"] , E )
              , (["7", "SW"], SW)
              , (["8", "S"] , S )
              , (["9", "SE"], SE)]
      toUppers = map toUpper
  in fmap snd . find (\(t, _) -> elem (toUppers s) (toUppers <$> t)) $ table

currentBoard :: Game Board
currentBoard = initLoop $ \env ->
  readIORef (envvalL `getL` env)

withCurrentBoard :: (Board -> (Board, a)) -> Game a
withCurrentBoard f = initLoop $ \env ->
  atomicModifyIORef (envvalL `getL` env) f

lastBoard :: Game Board
lastBoard = Loop $ \_ (s, t) ->
  return (s, (s, t))

putBoard :: Board -> Game ()
putBoard s = Loop $ \_ (_, t) ->
  return ((), (s, t))

modifyFinishedGames ::(FinishedGames -> FinishedGames)
                    -> Game ()
modifyFinishedGames f = Loop $ \_ (s, t) ->
  return ((), (s, f t))

finishedGames :: Game FinishedGames
finishedGames = Loop $ \_ (s, t) ->
  return (t, (s, t))

eGetLine :: Game String
eGetLine = initLoop (hGetLine . getL handleL)

ePutStrLn :: String -> Game ()
ePutStrLn s = initLoop (\env -> hPutStrLn (handleL `getL` env) s)

allClients :: Game (Set Ref)
allClients = initLoop $ \env ->
  readIORef $ clientsL `getL` env

process :: Command -> Game ()
process (Move position) = error "TODO"
process Current         = currentBoard >>= printFormatted
process Finished        = finishedGames >>= printFormatted
process (Chat msg)      = allClientsButThis ! formatResponse msg
process Turn            = currentBoard >>= printFormatted . whoseTurn
process (At position)   = currentBoard >>= printFormatted . whoOccupies position
process (Unknown commd) = pPutStrLnFormatted $ "Unknown command: " ++ commd

game :: Game x -- client accepted (post)
     -> (String -> Game w) -- read line from client
     -> IO a
game ca rl = iorefLoop
             b
             (b, [])
             ca
             rl
  where b = Board (singleton C Naught) [(C, Naught)]

play :: IO a
play = game (currentBoard >>= printFormatted)
         (process . command)

printFormatted :: Show a => a -> IOLoop v s ()
printFormatted = pPutStrLnFormatted . show

pPutStrLnFormatted :: String -> IOLoop v s ()
pPutStrLnFormatted = pPutStrLn . formatResponse

formatResponse :: String -> String
formatResponse = unlines . map ("> " ++) . lines
