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

type Tag      = Channel
type NickName = String

data Question = Question
    { qFull     :: String
    , qAsker    :: NickName
    , qMessages :: [Message]
    , qParent   :: Channel
    }
    
data BotState = BotState
    { botQuestions    :: Map Channel [Tag]
    , botTags         :: Map Tag Question
    , botAdmins       :: [UserName]
    }

newBotState :: [Channel] -> [UserName] -> BotState
newBotState chans admins =
    BotState { botQuestions = Map.fromList $ map (, []) chans
             , botTags      = Map.empty
             , botAdmins    = admins
             }

data UserCommand
    = NewQuestion (Maybe Tag) String

tok :: Parser a -> Parser a
tok p = p <* spaces

p_tag :: Parser String
p_tag = (:) <$> char '#' <*> ((:) <$> letter <*> many alphaNum)

p_userCommand :: Parser UserCommand
p_userCommand = p_newQuestion
  where
    p_newQuestion =
        NewQuestion
            <$> (tok (string "??") >> optionMaybe p_tag)
            <*> (manyTill anyChar eof)

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

drLogos :: BotProcess BotState
drLogos Message { msg_prefix  = Just (NickName nn _ _)
                , msg_command = "PRIVMSG"
                , msg_params  = [chan, msg]
                }
    | Just (NewQuestion tagM body) <- parseRes = newQuestion chan nn tagM body
    | otherwise = do
        mq <- Map.lookup chan <$> gets botTags
        case mq of
            Nothing -> return []
            Just (Question {qParent = parent}) -> do
                let parentMsg = "<" ++ nn ++ "> on " ++ chan ++ ": " ++ msg
                return [ privmsg parent parentMsg ]
  where
    parseRes = parse' p_userCommand msg
drLogos _msg = return []

initDrLogos :: MonadIRC m => BotState -> m BotState
initDrLogos bs@BotState {botQuestions = qs} =
    mapM_ (sendMessage . joinChan) (Map.keys qs) >>
    return bs
