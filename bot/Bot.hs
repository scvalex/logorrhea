{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bot 
    ( Tag
    , Question (..)
    , BotState (..)
    , newBotState
    , Bot (..)      
    , runBot      
    , BotProcess
    , botProcess
    ) where

import Control.Applicative
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import IRC

type Tag = Channel

data Question = Question
    { qFull     :: String
    , qAsker    :: UserName
    , qMessages :: [Message]
    }
    
data BotState = BotState
    { botQuestions :: Map Channel [Tag]
    , botTags      :: Map Tag Question
    , botChannels  :: [Channel]
    , botAdmins    :: [UserName]
    }

newBotState :: [Channel] -> [UserName] -> BotState
newBotState chans admins = BotState { botQuestions = Map.empty
                                    , botTags      = Map.empty
                                    , botChannels  = chans
                                    , botAdmins    = admins
                                    }

newtype Bot a = Bot {unBot :: State BotState a}
    deriving (Functor, Applicative, Monad, MonadState BotState)

runBot :: Bot a -> BotState -> (a, BotState)
runBot (Bot s) bs = runState s bs

type BotProcess = Message -> Bot [Message]

botProcess :: MonadIRC m => BotProcess -> BotState -> m ()
botProcess proc bs = forever $ do
    msg <- popMessage
    let (replies, bs') = runBot (proc msg) bs
    mapM sendMessage replies
    botProcess proc bs'