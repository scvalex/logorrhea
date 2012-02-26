{-# LANGUAGE TypeFamilies #-}

module Event( parseInEvent
            , parseOutEvent
            , InEvent(..)
            , OutEvent(..)
            , Response(..)
            , ServerIssued(..)) where

import Data.Aeson
import Data.Text

import IRC

type Reason = String

data InEvent
    = Connect NickName
    | DisConnect
    | ListChannels              -- monitored
    | ListUsers Channel
    | ListConversations Channel
    | Join Channel
    | SendChannel Channel String
    | SendConversation Channel Tag String


-- responses
data Response
    = ConnectOk
    | DisConnectOk
    | ListChannelsOk [Channel]
    | ListUsersOk [NickName]
    | ListConversationsOk [Tag]
    | JoinOk Channel
    | SendChannelOk
    | SendConversationOk

data ServerIssued
    = DisConnectS Reason
    | ReceiveChannel Channel NickName String
    | ReceiveConversation Channel NickName Tag String

data OutEvent
    = ServerIssued ServerIssued
    | Response Response


parseInEvent :: Text -> Maybe InEvent
parseInEvent = undefined

parseOutEvent :: Text -> Maybe OutEvent
parseOutEvent = undefined
