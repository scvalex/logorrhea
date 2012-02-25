{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IRC 
    ( IRCInfo (..)
    , MonadIRC (..)
    , IRCT
    , runIRCT

    , module Network.IRC.Base
    , module Network.IRC.Commands
    ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Network
import Network.IRC.Base
import Network.IRC.Commands
import System.IO

import qualified ParseMessage as Parse

data IRCInfo = IRCInfo
    { ircHandle :: Handle 
    , ircName   :: String
    , ircPort   :: PortNumber
    }

class (Applicative m, Monad m) => MonadIRC m where
    popMessage  :: m Message
    sendMessage :: Message -> m ()
    getUserName :: m UserName

newtype IRCT m a = IRCT {unIRC :: ReaderT IRCInfo (StateT UserName m) a}
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans IRCT where
    lift = IRCT . lift . lift

runIRCT :: MonadIO m => UserName -> IRCInfo -> IRCT m () -> m ()
runIRCT un i (IRCT act) = evalStateT (runReaderT act i) un

decode :: Monad m => ByteString -> m Message
decode bs = let Just msg = Parse.decode bs in return msg

instance (Applicative m, MonadIO m) => MonadIRC (IRCT m) where
    popMessage = IRCT $ do
        h <- asks ircHandle
        msg <- decode =<< liftIO (B.hGetLine h)
        return msg
    
    sendMessage msg = do
        h <- IRCT $ asks ircHandle
        let bs = B8.pack . (++"\r\n") . encode $ msg
        liftIO $ B.putStr bs
        liftIO (B.hPut h bs >> hFlush h)
        
    getUserName = IRCT . lift $ get
