{-# LANGUAGE DataKinds #-}

module Event where

import Data.Aeson

data Event
    = Connect
    | DisConnect
    | ListUsers
    | ListChannels
    | ListConversations
    | Join
    | SendChannel
    | SendConversation
    | 

data InEvent =
    InEvent Event (