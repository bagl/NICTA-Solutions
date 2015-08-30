module Network.Server.Chat.Chat where

import Network.Server.Common.Line
import Network.Server.Common.Ref (Ref)
import Network.Server.Chat.Loop
import Data.Maybe(fromMaybe)
import Data.Foldable(msum, Foldable)
import Data.IORef(atomicModifyIORef)
import Control.Applicative(pure, (<$>))
import Control.Monad.Trans(MonadIO(..))
import Text.Read (readMaybe)

type Chat a = IORefLoop Integer a

data ChatCommand = Chat String
                 | Add Integer
                 | Unknown String
                 deriving (Eq, Show)

chat :: IO a
chat = iorefLoop 0 (pure ()) (process . chatCommand)

-- |
--
-- >>> chatCommand "CHAT hi"
-- Chat "hi"
--
-- >>> chatCommand "Chat bye"
-- Chat "bye"
--
-- >>> chatCommand "ADD 65"
-- Add 65
--
-- >>> chatCommand "ADD failure"
-- UNKNOWN "ADD failure"
--
-- >>> chatCommand "Nothing"
-- UNKNOWN "Nothing"
chatCommand :: String -> ChatCommand
chatCommand z = fromMaybe (Unknown z)
  $ msum [ Chat <$> trimPrefixThen "CHAT" z
         , Add  <$> (trimPrefixThen "Add" z >>= readMaybe) ]

process :: ChatCommand -> Chat ()
process (Unknown s) = handleUnknownCommand s
process (Chat s)    = handleChatCommand s
process (Add n)     = handleAddCommand n

handleChatCommand :: String -> Chat ()
handleChatCommand = notifyAllButThis

handleUnknownCommand :: String -> Chat ()
handleUnknownCommand s = notifyThis $ "unknown command: " ++ s

handleAddCommand :: Integer -> Chat ()
handleAddCommand n = do
  e  <- readEnvval
  n' <- liftIO $ atomicModifyIORef e (\x -> (x + n, x + n))
  notifyAll $ "counter is at " ++ show n'

formatResponse :: String -> String
formatResponse = ("> " ++)

notifyAll :: String -> Chat ()
notifyAll = notify allClients

notifyAllButThis :: String -> Chat ()
notifyAllButThis = notify allClientsButThis

notifyThis :: String -> Chat ()
notifyThis = pPutStrLn . formatResponse

notify :: Foldable t => IOLoop v (t Ref) -> String -> IOLoop v ()
notify cs s = cs ! formatResponse s
