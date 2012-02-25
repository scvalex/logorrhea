{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IRC 
    ( IRCInfo (..)
    , MonadIRC (..)
    , IRCT
    , runIRCT
      
    , module Network.IRC.Base
    , module Network.IRC.Commands


    , dumpContents
    ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Network
import Network.IRC.Base
import Network.IRC.Commands
import System.IO

import ParseMessage 

data IRCInfo = IRCInfo
    { ircHandle :: Handle 
    , ircName   :: String
    , ircPort   :: PortNumber
    }

class MonadIRC m where
    popMessage  :: m Message
    sendMessage :: Message -> m ()

newtype IRCT m a = IRCT {unIRC :: ReaderT IRCInfo (StateT ByteString m) a}
    deriving (Functor, Monad, MonadIO)

instance MonadTrans IRCT where
    lift = IRCT . lift . lift 

runIRCT :: MonadIO m => IRCInfo -> IRCT m () -> m ()
runIRCT i@(IRCInfo { ircHandle = h }) (IRCT r) = do
    contents <- liftIO $ L.hGetContents h
    evalStateT (runReaderT r i) contents
    
dumpContents :: MonadIO m => IRCT m ()
dumpContents = do
    h <- IRCT $ asks ircHandle
    liftIO (L.hGetContents h >>= L.putStrLn)

instance MonadIO m => MonadIRC (IRCT m) where
    popMessage = IRCT $ do
        Just m <- liftM decode $ lift get
        return m
    
    sendMessage msg = do
        h <- IRCT $ asks ircHandle
        let bs = L8.pack . (++"\r\n") . encode $ msg
        liftIO $ L.putStr bs
        liftIO (L.hPut h bs >> hFlush h)
