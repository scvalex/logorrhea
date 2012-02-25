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
import Text.Parsec hiding (many)
import Text.Parsec.String

import Bot
import IRC

-- DEBUG
import Debug.Trace

type Tag      = Channel
type NickName = String

data Question = Question
    { qFull     :: String
    , qAsker    :: NickName
    , qMessages :: [Message]
    , qParent   :: Channel
    , qTag :: Tag
    }
    deriving (Show)

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

data UserCommand
    = NewQuestion (Maybe Tag) String
    | ListQuestions

tok :: Parser a -> Parser a
tok p = p <* space <* spaces

p_startCommand :: Parser ()
p_startCommand = string "??" >> return ()

p_tag :: Parser String
p_tag = (:) <$> char '#' <*> ((:) <$> letter <*> many alphaNum)

p_userCommand :: Parser UserCommand
p_userCommand = choice . map try $
                [ p_newQuestion
                , p_listQuestions ]
  where
    p_newQuestion =
        NewQuestion
            <$> (tok p_startCommand >> optionMaybe p_tag)
            <*> (manyTill anyChar eof)
    p_listQuestions =
        p_startCommand >> string "list" >> spaces >> eof >> return ListQuestions


parse' :: Parser a -> String -> Maybe a
parse' p s = either (const Nothing) Just $ parse p "" s

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
        ad       = "<" ++ nn ++ "> asked: " ++ body
        parentAd = ad ++ ". Join " ++ tag ++ " to help him/her."
    put bs {botQuestions = qs', botTags = tags'}
    return [ joinChan tag
           , privmsg tag ad
           , privmsg chan parentAd
           ]

listQuestions :: NickName -> Channel -> Bot BotState [Message]
listQuestions nn chan = do
    tags <- gets botTags
    questions <- map (tags Map.!) . (Map.! chan) <$> gets botQuestions
    let questionDesc q =
            qTag q ++ " <" ++ qAsker q ++ "> " ++ qFull q
    trace (show questions) $ return . map (privmsg nn . questionDesc) $ questions

drLogos :: BotProcess BotState
drLogos wholeMsg@Message { msg_prefix  = Just (NickName nn _ _)
                         , msg_command = "PRIVMSG"
                         , msg_params  = [chan, msg]
                         }
    | Just (NewQuestion tagM body) <- parseRes = newQuestion chan nn tagM body
    | Just ListQuestions <- parseRes = listQuestions nn chan
    | otherwise = do
        mq <- Map.lookup chan <$> gets botTags
        case mq of
            Nothing -> return []
            Just q@(Question { qParent   = parent
                             , qMessages = messages}) -> do
                let parentMsg = "<" ++ nn ++ "> on " ++ chan ++ ": " ++ msg
                bs@BotState {botTags = tags} <- get
                put bs {botTags = Map.insert chan
                                  q {qMessages = wholeMsg : messages} tags}
                return [ privmsg parent parentMsg ]
  where
    parseRes = parse' p_userCommand msg
drLogos _msg = return []

initDrLogos :: MonadIRC m => BotState -> m BotState
initDrLogos bs@BotState {botQuestions = qs} =
    mapM_ (sendMessage . joinChan) (Map.keys qs) >>
    return bs
