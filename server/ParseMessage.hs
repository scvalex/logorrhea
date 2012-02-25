-- | Parsec parsers and a general parsing interface for IRC messages
module ParseMessage (
    -- * Parsing and Formatting Functions
    decode -- :: ByteString -> Maybe Message
  ) where

import Network.IRC.Base

import Data.ByteString.Lazy (ByteString)
import Control.Monad
import Data.Maybe
import Text.Parsec hiding (spaces)
import Text.Parsec.ByteString.Lazy

-- | Parse a String into a Message.
decode :: ByteString    -- ^ Message string
       -> Maybe Message -- ^ Parsed message
decode = (either (const Nothing) Just) . (parse message "")

-- | Take all tokens until one character from a given string is found
takeUntil :: String -> Parser String
takeUntil s = anyChar `manyTill` (lookAhead (oneOf s))

-- | Convert a parser that consumes all space after it
tokenize  :: Parser a -> Parser a
tokenize p = p >>= \x -> spaces >> return x

-- | Consume only spaces tabs or the bell character
spaces :: Parser ()
spaces  = skipMany1 (oneOf " \t\b")

-- | Parse a Prefix
prefix :: Parser Prefix
prefix  = char ':' >> (try nicknamePrefix <|> serverPrefix)

-- | Parse a Server prefix
serverPrefix :: Parser Prefix
serverPrefix  = takeUntil " " >>= return . Server

-- | Parse a NickName prefix
nicknamePrefix :: Parser Prefix
nicknamePrefix  = do
  n <- takeUntil " .!@\r\n"
  p <- option False (char '.' >> return True)
  when p (fail "")
  u <- optionMaybe $ char '!' >> takeUntil " @\r\n"
  s <- optionMaybe $ char '@' >> takeUntil " \r\n"
  return $ NickName n u s

-- | Parse a command.  Either a string of capital letters, or 3 digits.
command :: Parser Command
command  = (many1 upper)
        <|> do x <- digit
               y <- digit
               z <- digit
               return [x,y,z]

-- | Parse a command parameter.
parameter :: Parser Parameter
parameter  =  (char ':' >> takeUntil "\r\n")
          <|> (takeUntil " \r\n")

-- | Parse a cr lf
crlf :: Parser ()
crlf  =  (char '\r' >> optional (char '\n'))
     <|> (char '\n' >> return ()           )


-- | Parse a Message
message :: Parser Message
message  = do
  p <- optionMaybe $ tokenize prefix
  c <- command
  ps <- many (spaces >> parameter)
  crlf >> eof
  return $ Message p c ps

