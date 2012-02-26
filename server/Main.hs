import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans
import Network.WebSockets
import Data.ByteString.Lazy (ByteString)

import Event
import IRC

type Prot = Hybi10

_IRCServer   = "irc.freenode.org"
_IRCPort     = fromIntegral 6667

main :: IO ()
main = runServer "0.0.0.0" 9999 server

server :: Request -> WebSockets Prot ()
server rq = do
    acceptRequest rq 
    connect <- receiveData :: WebSockets Prot ByteString
    case parseInEvent connect of
        Just (Connect nn) -> do
            let conn = liftIO $ connectToServer _IRCServer _IRCPort
            withConnection nn conn $ do
                lift getVersion >>= liftIO . putStrLn . ("Client version: " ++)
                sink <- lift getSink
                forkIRC (ircProxy sink)
                forever $ do
                    undefined       -- get message from frontend
        Nothing -> undefined

ircProxy :: MonadIRC m => Sink Prot -> m ()
ircProxy sink = do
    undefined
