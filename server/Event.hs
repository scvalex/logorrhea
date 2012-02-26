{-# LANGUAGE TypeFamilies, OverloadedStrings #-}

module Event
    ( parseInEvent
    , unParseOutEvent
    , InEvent(..)
    , OutEvent(..)
    , Response(..)
    , ServerIssued(..)
    ) where

import Control.Applicative
import Control.Monad hiding (join)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Lazy as HashMap
import Data.Text (Text)
import qualified Data.Text as Text

import IRC hiding (encode)

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
    | Response (Either (InEvent, String) Response)
    | GenericError

parseInEvent :: ByteString -> Maybe InEvent
parseInEvent t = do
    j <- decode t :: Maybe Value
    case j of
        Object obj -> parseObject $ HashMap.toList obj
        _          -> mzero
  where
    connect [("user", String nn)] = return . Connect . Text.unpack $  nn
    connect _                     = mzero
    
    list_channels [] = return ListChannels
    list_channels _  = mzero
    
    list_users [("channel", String chan)] =
        return . ListUsers . Text.unpack $ chan
    list_users _                          = mzero
    
    list_conversations [("channel", String chan)] = 
        return . ListConversations . Text.unpack $ chan
    list_conversations _                          = mzero
    
    join [("channel", String chan)] =
        return . Join . Text.unpack $ chan
    join _                          = mzero
    
    send_channel [("channel", String chan), ("message", String msg)] =
        return $ SendChannel (Text.unpack chan) (Text.unpack msg)
    send_channel _ = undefined
    
    send_conversation [ ("channel",      String chan)
                      , ("conversation", String convo) 
                      , ("message",      String msg)
                      ] =
        SendConversation <$> return (Text.unpack chan)
                         <*> return (Text.unpack convo)
                         <*> return (Text.unpack msg)
    send_conversation _ = mzero
    
    parseObject [("event", "connect"), ("data", Object data')] =
        connect $ HashMap.toList data'
    parseObject [("event", "disconnect"), ("data", Object data')]
        | data' == HashMap.empty = return DisConnect
        | otherwise              = mzero
    parseObject [("event", "list_channels"), ("data", Object data')] =
        list_channels $ HashMap.toList data'
    parseObject [("event", "list_users"), ("data", Object data')] =
        list_users $ HashMap.toList data'
    parseObject [("event", "list_conversations"), ("data", Object data')] =
        list_conversations $ HashMap.toList data'
    parseObject [("event", "join"), ("data", Object data')] =
        join $ HashMap.toList data'
    parseObject [("event", "send_channel"), ("data", Object data')] =
        send_channel $ HashMap.toList data'
    parseObject [("event", "send_converstation"), ("data", Object data')] =
        send_conversation $ HashMap.toList data'
    parseObject _ = mzero
    
unParseOutEvent :: OutEvent -> ByteString
unParseOutEvent (ServerIssued si) = encode . unParseServerIssued $ si
unParseOutEvent (Response (Left (Connect _, err))) =  undefined
unParseOutEvent (Response (Left (DisConnect, err))) =  undefined
unParseOutEvent (Response (Left (ListChannels, err))) =  undefined
unParseOutEvent (Response (Left (ListUsers _, err))) =  undefined
unParseOutEvent (Response (Left (ListConversations _, err))) =  undefined
unParseOutEvent (Response (Left (Join _, err))) =  undefined
unParseOutEvent (Response (Left (SendChannel _ _, err))) =  undefined
unParseOutEvent (Response (Left (SendConversation _ _ _, err))) =  undefined
unParseOutEvent (Response (Right resp)) = encode . unParseResponse $ resp
unParseOutEvent GenericError =
    encode $ object [ "event"  .= ("error" :: Text)
                    , "reason" .= ("We don't know" :: Text)
                    ]

unParseResponse :: Response -> Value
unParseResponse ConnectOk =
    object [ "event" .= ("connect.ok" :: Text)
           , "data"  .= object []
           ]
unParseResponse DisConnectOk =
    object [ "event" .= ("disconnect.ok" :: Text)
           , "data"  .= object []
           ]
unParseResponse (ListChannelsOk chans) =
    object [ "event" .= ("list_channels.ok" :: Text)
           , "data"  .= chans
           ]
unParseResponse (ListUsersOk users) =
    object [ "event" .= ("list_users.ok" :: Text)
           , "data"  .= users
           ]
unParseResponse (ListConversationsOk convos) =
    object [ "event" .= ("list_conversations.ok" :: Text)
           , "data"  .= convos
           ]
unParseResponse (JoinOk chan) =
    object [ "event" .= ("join.ok" :: Text)
           , "data"  .= chan
           ]
unParseResponse SendChannelOk =
    object [ "event" .= ("send_channel.ok" :: Text)
           , "data"  .= object []
           ]
unParseResponse SendConversationOk =
    object [ "event" .= ("send_conversation.ok" :: Text)
           , "data"  .= object []
           ]

unParseServerIssued :: ServerIssued -> Value
unParseServerIssued (DisConnectS reason) =
    object [ "event" .= ("disconnect" :: Text)
           , "data"  .= object []
           ]
unParseServerIssued (ReceiveChannel chan nn msg) =
    object [ "event" .= ("receive_channel" :: Text)
           , "data"  .= object [ "channel" .= chan
                               , "user"    .= nn
                               , "message" .= msg
                               ]
           ]
unParseServerIssued (ReceiveConversation chan nn tag msg) =
    object [ "event" .= ("receive_conversation" :: Text)
           , "data"  .= object [ "channel"      .= chan
                               , "user"         .= nn
                               , "conversation" .= tag
                               , "message"      .= msg
                               ]
           ]
