{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class
import Control.Monad
import qualified Network.WebSockets as WS
import Data.Text

type Protocol = WS.Hybi10

main :: IO ()
main = do
    WS.runServer "0.0.0.0" 9999 pong


pong :: WS.Request -> WS.WebSockets Protocol ()
pong rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    WS.sendTextData ("Hello there" :: Text)
    forever $ do
        asd <- WS.receiveData :: WS.WebSockets Protocol Text 
        liftIO $ print asd
        WS.sendTextData asd
    