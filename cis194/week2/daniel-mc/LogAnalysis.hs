{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import           Log

safeRead :: Read a => String -> Maybe a
safeRead x = case reads x of
  [(r, "")] -> Just r
  _         -> Nothing

getMessageType :: [String] -> Maybe MessageType
getMessageType ("I"          : _) = Just Info
getMessageType ("W"          : _) = Just Warning
getMessageType ("E" : lvlStr : _) = case safeRead lvlStr of
  Just lvl -> Just $ Error lvl
  Nothing  -> Nothing
getMessageType _ = Nothing

getMessageTsStr :: [String] -> Maybe String
getMessageTsStr ("E" : _ : tsStr : _) = Just tsStr
getMessageTsStr (_       : tsStr : _) = Just tsStr
getMessageTsStr _                     = Nothing

getMessageTs :: [String] -> Maybe TimeStamp
getMessageTs msg = case getMessageTsStr msg of
  Just tsStr -> safeRead tsStr
  Nothing    -> Nothing

getMessageTextArr :: [String] -> [String]
getMessageTextArr ("E" : rest) = drop 2 rest
getMessageTextArr msg          = drop 2 msg

getMessageText :: [String] -> String
getMessageText = unwords . getMessageTextArr

parseMessage :: String -> LogMessage
parseMessage msg =
  let msgArr = words msg
  in  case (getMessageType msgArr, getMessageTs msgArr) of
        (Just msgType, Just msgTs) -> LogMessage msgType msgTs (getMessageText msgArr)
        _                          -> Unknown msg

parseFile :: String -> [LogMessage]
parseFile x = map parseMessage (lines x)
