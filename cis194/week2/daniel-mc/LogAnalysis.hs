{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import           Log

safeRead :: Read a => String -> Maybe a
safeRead x = case reads x of
  [(r, "")] -> Just r
  _         -> Nothing

-- Exercise 1

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

parse :: String -> [LogMessage]
parse x = map parseMessage (lines x)

-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert msg@LogMessage{} Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _) (Node left root@(LogMessage _ ts' _) right)
  | ts < ts'  = Node (insert msg left) root right
  | otherwise = Node left root (insert msg right)
insert _ mt = mt

-- Exercise 3

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                   = []
inOrder (Node left root right) = inOrder left ++ [root] ++ inOrder right

-- Exercise 5

filterRelevantInfo :: [LogMessage] -> [LogMessage]
filterRelevantInfo [] = []
filterRelevantInfo (x : xs) =
  let rest = filterRelevantInfo xs
  in  case x of
        (LogMessage (Error lvl) _ _) | lvl >= 50 -> x : rest
                                     | otherwise -> rest
        _ -> rest

-- gets message strings for LogMessages, ignoring Unknown
getMessages :: [LogMessage] -> [String]
getMessages (LogMessage _ _ msg : rest) = msg : getMessages rest
getMessages (_                  : rest) = getMessages rest
getMessages _                           = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = getMessages . inOrder . build . filterRelevantInfo
