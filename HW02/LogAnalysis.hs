module Main where

import Log
import Text.Read

main :: IO ()
main = undefined

parseMessageType :: [String] -> Maybe (MessageType, [String])
parseMessageType ("E":n:rest) = case readMaybe n :: Maybe Int of
  Just x -> Just (Error x, rest)
  Nothing -> Nothing
parseMessageType ("I":rest) = Just (Info, rest)
parseMessageType ("W":rest) = Just (Warning, rest)
parseMessageType _ = Nothing

-- Parses an individual line from the log file.
--
-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- parseMessage "This is not in the right format"
--   == Unknown "This is not in the right format"
parseMessage :: String -> LogMessage
parseMessage s = case parseMessageType (words s) of
  Just (typ, time:msg) -> case readMaybe time :: Maybe Int of
    Just x -> LogMessage typ x (unwords msg)
    Nothing -> Unknown s
  Nothing -> Unknown s

-- Parse an entire log file at once.
parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ t1 _) (Node left node@(LogMessage _ t2 _) right)
  | t1 < t2 = Node (insert msg left) node right
  | otherwise = Node left node (insert msg right)

build :: [LogMessage] -> MessageTree
build = build' Leaf
  where build' :: MessageTree -> [LogMessage] -> MessageTree
        build' tree [] = tree
        build' tree (msg:msgs) = build' (insert msg tree) msgs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = getMsgStrs . inOrder . build . filterMsgs

filterMsgs :: [LogMessage] -> [LogMessage]
filterMsgs = filter filterMsg
  where filterMsg :: LogMessage -> Bool
        filterMsg (LogMessage (Error x) _ _) = x >= 50
        filterMsg _ = False

getMsgStrs :: [LogMessage] -> [String]
getMsgStrs = map msgStr
  where msgStr :: LogMessage -> String
        msgStr (LogMessage _ _ m) = m
        msgStr _ = "Unknown"
