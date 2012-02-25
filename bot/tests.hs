import Control.Monad
import Control.Monad.Trans
import Network
import System.IO

import IRC

server   = "irc.freenode.org"
port     = fromIntegral 6667
chan     = "#testlogo"
nickName = "maaantrh5rhguyr"

connectToServer :: String -> PortNumber -> IO IRCInfo
connectToServer server port = do
    h <- connectTo server (PortNumber port)
    hSetBuffering h (BlockBuffering Nothing)
    return IRCInfo { ircHandle = h
                   , ircName   = server
                   , ircPort   = port
                   }

test_connectJoin :: IO ()
test_connectJoin = withSocketsDo $ do
    info <- connectToServer server port
    runIRCT info $ do
        mapM_ sendMessage
            [ nick nickName
            , user nickName "*" "*" nickName
            , joinChan chan
            ]
        forever $ popMessage >>= liftIO . print
    hClose $ ircHandle info
        

main :: IO ()
main = do
    test_connectJoin
