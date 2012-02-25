import Control.Monad
import Control.Monad.Trans
import Network
import System.IO
import System.Environment

import IRC

server   = "irc.freenode.org"
port     = fromIntegral 6667
chan     = "#testlogo"
userName = "maaantrh5rhguyr"

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
        nick un
        user un "*" "*" un
        act
        quit Nothing
        return ()

test_connectJoin :: IRCT IO ()
test_connectJoin = do
    joinChan chan
    forever $ popMessage >>= liftIO . putStrLn . encode

test_connectJoinMsg :: IRCT IO ()
test_connectJoinMsg = do
    joinChan chan
    privmsg chan "hello!"
    part chan
    forever $ popMessage >>= liftIO . putStrLn . encode
    
main :: IO ()
main = do
    args <- getArgs
    let un = case args of
            [s] -> s
            _   -> userName
        conn = connectToServer server port
    -- withConnection un conn test_connectJoin
    withConnection un conn test_connectJoinMsg
