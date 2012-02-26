{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
module Bot 
    ( Bot (..)      
    , runBot      
    , BotProcess
    , botProcess
    ) where

import Control.Applicative
import Control.Monad.State

import IRC

newtype Bot s a = Bot {unBot :: State s a}
    deriving (Functor, Applicative, Monad, MonadState s)

runBot :: Bot s a -> s -> (a, s)
runBot (Bot s) bs = runState s bs

type BotProcess s = Message -> Bot s [Message]

botProcess :: (MonadIO m, MonadIRC m) => BotProcess s -> s -> m ()
botProcess proc bs = forever $ do
    msg <- popMessage
    liftIO . putStrLn . encode $ msg
    case msg of
        Message { msg_prefix = Nothing
                , msg_command = "PING"
                , msg_params = params } ->
            sendMessage (Message Nothing "PONG" params) >>
            botProcess proc bs
        _ -> let (replies, bs') = runBot (proc msg) bs
             in  sendMessages replies >> botProcess proc bs'
