
import Control.Monad
import Control.Monad.Trans
import Network
import System.IO
import System.Environment

import IRC
import Bot
import DrLogos

botState = newBotState ["#testlogo"] []

server   = "irc.freenode.org"
port     = fromIntegral 6667
userName = "maaantrh5rhguasd"

connectToServer :: String -> PortNumber -> IO IRCInfo
connectToServer server port = do
    h <- connectTo server (PortNumber port)
    hSetBuffering h (BlockBuffering Nothing)
    return IRCInfo { ircHandle = h
                   , ircName   = server
                   , ircPort   = port
                   }
    
withConnection :: UserName -> IO IRCInfo -> IRCT IO () -> IO ()
withConnection un conn act = do
    i <- conn
    runIRCT un i $ do
        sendMessage $ nick un
        sendMessage $ user un "*" "*" un
        act
        sendMessage $ quit Nothing
        return ()

main :: IO ()
main = do
    args <- getArgs
    let un = case args of
            [s] -> s
            _   -> userName
        conn = connectToServer server port
    withConnection un conn (initDrLogos botState >>= botProcess drLogos)
