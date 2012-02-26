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
userName = "DrLogos"

main :: IO ()
main = do
    args <- getArgs
    let un = case args of
            [s] -> s
            _   -> userName
        conn = connectToServer server port
    withConnection un conn (initDrLogos botState >>= botProcess drLogos)
