{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import           Log

safeRead :: Read a => String -> Maybe a
safeRead x = case reads x of
  [(r, "")] -> Just r
  _         -> Nothing

data MessageTypeResult = MessageTypeSuccess MessageType [String]
                       | MessageTypeFailure             [String]
  deriving Show

extractMessageType :: [String] -> MessageTypeResult
extractMessageType (     "I"          : msg) = MessageTypeSuccess Info msg
extractMessageType (     "W"          : msg) = MessageTypeSuccess Warning msg
extractMessageType msg'@("E" : lvlStr : msg) = case safeRead lvlStr of
  Just lvl -> MessageTypeSuccess (Error lvl) msg
  Nothing  -> MessageTypeFailure msg'
extractMessageType msg = MessageTypeFailure msg

parseMessageOfType :: MessageTypeResult -> LogMessage
parseMessageOfType (MessageTypeFailure msg) = Unknown $ unwords msg
parseMessageOfType (MessageTypeSuccess msgType msg'@(tsStr : msg)) =
  case (safeRead tsStr :: Maybe TimeStamp) of
    Just ts -> LogMessage msgType ts (unwords msg)
    Nothing -> Unknown (unwords msg')
parseMessageOfType (MessageTypeSuccess _ msg) = Unknown $ unwords msg

parseMessage :: String -> LogMessage
parseMessage = parseMessageOfType . extractMessageType . words

-- (I | W | E Int) Int ...

-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- parseMessage "This is not in the right format" == Unknown "This is not in the right format"
-- parseMessage "I 29 la la la" == Unknown "I 29 la la la"
