{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans
import Network.WebSockets hiding (Response)
import qualified Data.ByteString.Lazy as L
import Text.Parsec
import Data.Function

import Event
import IRC
import DrLogos.Parser

type Prot = Hybi10

_IRCServer   = "irc.freenode.org"
_IRCPort     = fromIntegral 6667

type WebIRC a = IRCT (WebSockets Prot) a


main :: IO ()
main = runServer "0.0.0.0" 9999 server

recData :: WebSockets Prot L.ByteString
recData = receiveData

outEvent :: OutEvent -> WebSockets Prot ()
outEvent = sendTextData . unParseOutEvent

server :: Request -> WebSockets Prot ()
server rq = do
    acceptRequest rq         
    connect <- recData
    liftIO $ L.putStrLn connect
    case parseInEvent connect of
        Just (Connect nn) -> do
            outEvent (Response (Right ConnectOk))
            let conn = liftIO $ connectToServer _IRCServer _IRCPort
            withConnection nn conn $ do
                lift getVersion >>= liftIO . putStrLn . ("Client version: " ++)
                sink <- lift getSink
                forkIRC (ircProxy sink)
                fix $ \go -> do
                    inEvent <- lift recData
                    case parseInEvent inEvent of
                        Nothing -> lift $ outEvent (GenericError "can't parse")
                        Just e -> handleEvent e go
        Just otherEvent -> do
            outEvent (Response (Left (otherEvent, "\"connect\" expected")))
        Nothing -> do
            outEvent (GenericError "\"connect\" expected")

uCmd :: UserCommand -> String
uCmd = unParse u_userCommand

handleEvent :: InEvent -> WebIRC () -> WebIRC ()
handleEvent event go = case event of
    Connect _ -> do
        lift $ outEvent (Response (Left (event, "multiple \"connect\""))) 
        go
    DisConnect -> do
        lift $ outEvent (Response (Right DisConnectOk))
    ListChannels -> sendMessage (privmsg "DrLogos" (uCmd ListMonitoredChannels)) >> go
    ListUsers chan -> do
        lift $ outEvent (Response (Right (ListUsersOk chan ["rostayob", "exFalso", "nh2", "scvalex"])))
        go
    ListConversations chan -> do
        sendMessage (privmsg chan (uCmd ListQuestions))
        go
    Join chan -> do
        sendMessage (joinChan chan)
        lift $ outEvent (Response (Right (JoinOk chan)))
        go
    SendChannel chan msg -> do
        sendMessage (privmsg chan msg)
        go
    SendConversation _ tag msg -> do
        sendMessage (privmsg tag msg)
        go

pBMsg :: String -> Maybe BotMessage
pBMsg = either (const Nothing) Just . parse p_botMessage ""

pUCmd :: String -> Maybe UserCommand
pUCmd = either (const Nothing) Just . parse p_userCommand ""

ircProxy :: Sink Prot -> IRCT IO ()
ircProxy sink = forever $ do
    let ss = liftIO . sendSink sink . DataMessage . Text . unParseOutEvent
    msg <- popMessage
    liftIO . putStrLn . encode $ msg
    case msg of 
        Message { msg_prefix = Just (NickName "DrLogos" _ _)
                , msg_command = "PRIVMSG"
                , msg_params = [chan, msg'] } -> case pBMsg msg' of
            Just (UserMessage nn t s) ->
                ss (ServerIssued (ReceiveConversation chan nn t s))
            Just (NoQuestions _) ->
                ss (Response (Right (ListConversationsOk [])))
            Just (NotAQuestion _) ->
                ss (Response (Left (ListConversations undefined,
                                    "this channel cannot have a conversation")))
            Just (HistoryMessage nn body) ->
                ss (ServerIssued (ReceiveConversation chan nn chan body))
            Just (MonitoredChannels chans) -> ss (Response (Right (ListChannelsOk chans)))
            Just (StartHistory _) -> return ()
            Just (EndHistory _) -> return ()
            Just (QuestionCreated nn t q) ->
                ss (ServerIssued (ReceiveConversation chan nn t q))
            Nothing -> return ()
        Message { msg_prefix = Just (NickName nn _ _)
                , msg_command = "PRIVMSG"
                , msg_params = [chan, msg'] } ->
            case pUCmd msg' of
                Nothing -> ss (ServerIssued (ReceiveConversation chan nn chan msg'))
                Just _  -> return ()
        _ -> return ()
