module DrLogos.BotMessage
    ( BotMessage (..)
    , p_botMessage
    , u_botMessage
    ) where


import Control.Applicative
import Text.Parsec hiding (many)
import Text.Parsec.String

sp :: Parser a -> Parser a
sp p = p <* space <* spaces

p_botMessage :: Parser BotMessage
p_botMessage = choice . map try $
               [ p_userMessage
               , p_noQuestions
               ]

p_userMessage :: Parser BotMessage
p_userMessage = UserMessage <$> sp p_nickName <*> sp (string "@")
             <*> sp p_tag <*> string ": " <*> manyTill anyChar eof

p_noQuestions :: Parser BotMessage
p_noQuestions = NoQuestions <$> (string "No questions in " >> p_tag)

p_nickName :: Parser NickName
p_nickName = (:) <$> letter <*> many alphaNum

p_tag :: Parser String
p_tag = (:) <$> char '#' <*> ((:) <$> letter <*> many alphaNum)

type UnParser a = a -> String -> String

unparse :: UnParser a -> a -> String
unparse up a = up a []

u_string :: UnParser String
u_string = (++)

       -- ??list
       -- rostayob @ #haskell : what is haskell?
       
u_botMessage :: UnParser BotMessage
u_botMessage (UserMessage n t s) = foldl (.) id . map u_string $  
                                   [n, " @ ", t, " : ", s]
u_botMessage (NoQuestions t) = u_string $ "No questions in " ++ t


