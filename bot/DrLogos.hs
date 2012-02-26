{-# LANGUAGE TupleSections #-}
module DrLogos
    ( Tag
    , NickName
    , Question (..)
    , BotState (..)
    , newBotState
    , drLogos
    , initDrLogos
    ) where

import Control.Applicative
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec (parse)
import Text.Parsec.String (Parser)

import DrLogos.Parser
import Bot
import IRC

data Question = Question
    { qFull     :: String
    , qAsker    :: NickName
    , qMessages :: [(NickName, String)]
    , qParent   :: Channel
    , qTag :: Tag
    }

data BotState = BotState
    { botQuestions    :: Map Channel [Tag]
    , botTags         :: Map Tag Question
    , botAdmins       :: [UserName]
    , botClosed       :: Map Tag Question
    }

newBotState :: [Channel] -> [UserName] -> BotState
newBotState chans admins =
    BotState { botQuestions = Map.fromList $ map (, []) chans
             , botTags      = Map.empty
             , botAdmins    = admins
             , botClosed    = Map.empty
             }

parse' :: Parser a -> String -> Maybe a
parse' p s = either (const Nothing) Just $ parse p "" s

unParseBM :: BotMessage -> String
unParseBM = unParse u_botMessage

-- FIXME: actually generate unique tag
createUniqueTag :: Maybe String -> Bot BotState Tag
createUniqueTag (Just t) = return t
createUniqueTag _        = error "FIXME: generate unique tag"

newQuestion :: Channel -> NickName -> Maybe String
            -> String -> Bot BotState [Message]
newQuestion chan nn tagM body = do
    tag <- createUniqueTag tagM
    bs@BotState {botQuestions = qs, botTags = tags} <- get
    let q        = Question { qFull     = body
                            , qAsker    = nn
                            , qMessages = []
                            , qParent   = chan
                            , qTag      = tag
                            }
        tags'    = Map.insert tag q tags              
        -- FIXME: Better error handling
        chanTags = qs Map.! chan
        qs'      = Map.insert chan (tag : chanTags) qs
        ad       = unParseBM $ UserMessage nn tag body
    put bs {botQuestions = qs', botTags = tags'}
    return [ joinChan tag
           , privmsg tag ad
           , privmsg chan ad
           ]

listQuestions :: NickName -> Channel -> Bot BotState [Message]
listQuestions nn chan = do
    chanTagsM <- Map.lookup chan <$> gets botQuestions
    let noQs = privmsg nn . unParseBM $ NoQuestions chan
    case chanTagsM of
        Nothing       -> return [noQs]
        Just chanTags ->
            case chanTags of
                [] -> return [noQs]
                _  -> do
                    tags <- gets botTags
                    let questions = map (tags Map.!) chanTags
                        questionDesc q =
                            unParseBM $ UserMessage (qAsker q) (qTag q) (qFull q)
                    return $ map (privmsg nn . questionDesc) questions

                        

questionHistory :: NickName -> Channel -> Bot BotState [Message]
questionHistory nn chan = do
    qM <- Map.lookup chan <$> gets botTags
    return $ case qM of
        Nothing -> [privmsg nn (unParse u_botMessage $ NotAQuestion chan)]
        Just Question {qMessages = msgs} ->
            let start = privmsg nn . unParseBM $ StartHistory chan
                end   = privmsg nn . unParseBM $ EndHistory chan
            in  start :
                map (privmsg nn . unParseBM . uncurry HistoryMessage) (reverse msgs) ++
                [end]
    
listMonitoredChannels :: NickName -> Bot BotState [Message]
listMonitoredChannels nn =
    (pure . privmsg nn . unParse u_botMessage . MonitoredChannels . Map.keys)
    <$> gets botQuestions

drLogos :: BotProcess BotState
drLogos Message { msg_prefix  = Just (NickName nn _ _)
                , msg_command = "PRIVMSG"
                , msg_params  = [chan, msg]
                }
    | Just (NewQuestion tagM body) <- parseRes = newQuestion chan nn tagM body
    | Just ListQuestions <- parseRes = listQuestions nn chan
    | Just (History tag) <- parseRes = questionHistory nn tag
    | Just ListMonitoredChannels <- parseRes = listMonitoredChannels nn
    | otherwise = do
        mq <- Map.lookup chan <$> gets botTags
        case mq of
            Nothing -> return []
            Just q@(Question { qParent   = parent
                             , qMessages = messages}) -> do
                let parentMsg = unParse u_botMessage $ UserMessage nn chan msg
                bs@BotState {botTags = tags} <- get
                put bs {botTags = Map.insert chan
                                  q {qMessages = (nn, msg) : messages} tags}
                return [ privmsg parent parentMsg ]
  where
    parseRes = parse' p_userCommand msg
drLogos _msg = return []

initDrLogos :: MonadIRC m => BotState -> m BotState
initDrLogos bs@BotState {botQuestions = qs} =
    mapM_ (sendMessage . joinChan) (Map.keys qs) >>
    return bs
