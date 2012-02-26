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
    deriving (Show)

-- responses
data Response
    = ConnectOk
    | DisConnectOk
    | ListChannelsOk [Channel]
    | ListUsersOk Channel [NickName]
    | ListConversationsOk [Tag]
    | JoinOk Channel
    | SendChannelOk
    | SendConversationOk
    deriving (Show)
             
data ServerIssued
    = DisConnectS Reason
    | ReceiveChannel Channel NickName String
    | ReceiveConversation Channel NickName Tag String
    deriving (Show)
             
data OutEvent
    = ServerIssued ServerIssued
    | Response (Either (InEvent, String) Response)
    | GenericError String
    deriving (Show)
             
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
    
    send_conversation [ ("channel", String chan)
                      , ("tag",     String convo) 
                      , ("message", String msg)
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
unParseOutEvent (Response (Left (Connect _, err))) =
    encode $ unParseError "connect" err
unParseOutEvent (Response (Left (DisConnect, err))) =
    encode $ unParseError "disconnect" err
unParseOutEvent (Response (Left (ListChannels, err))) =
    encode $ unParseError "list_channels" err
unParseOutEvent (Response (Left (ListUsers _, err))) =
    encode $ unParseError "list_users" err
unParseOutEvent (Response (Left (ListConversations _, err))) =
    encode $ unParseError "list_conversations" err
unParseOutEvent (Response (Left (Join _, err))) =
    encode $ unParseError "join" err
unParseOutEvent (Response (Left (SendChannel _ _, err))) =
    encode $ unParseError "send_channel" err
unParseOutEvent (Response (Left (SendConversation _ _ _, err))) =
    encode $ unParseError "send_conversation" err
unParseOutEvent (Response (Right resp)) = encode . unParseResponse $ resp
unParseOutEvent (GenericError str) =
    encode $ object [ "event"  .= ("error" :: Text)
                    , "reason" .= str
                    ]

unParseError :: String -> String -> Value
unParseError ev err = object ["event" .= ev, "error" .= err]

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
           , "data"  .= object ["channels" .= chans]
           ]
unParseResponse (ListUsersOk chan users) =
    object [ "event" .= ("list_users.ok" :: Text)
           , "data"  .= object ["channel" .= chan, "users" .= users]
           ]
unParseResponse (ListConversationsOk convos) =
    object [ "event" .= ("list_conversations.ok" :: Text)
           , "data"  .= object ["tags" .= convos]
           ]
unParseResponse (JoinOk chan) =
    object [ "event" .= ("join.ok" :: Text)
           , "data"  .= object ["channel" .= chan]
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
           , "data"  .= object ["reason" .= reason]
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
           , "data"  .= object [ "channel" .= chan
                               , "user"    .= nn
                               , "tag"     .= tag
                               , "message" .= msg
                               ]
           ]
