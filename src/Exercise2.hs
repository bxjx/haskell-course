module Exercise2 where

import Log

parseMessage :: String -> LogMessage
parseMessage logMessage = case words logMessage of
  ( "I" : timestamp : rest) -> LogMessage Info (read timestamp) (unwords rest)
  ( "W" : timestamp : rest) -> LogMessage Warning (read timestamp) (unwords rest)
  ( "E" : priority :timestamp : rest) -> LogMessage (Error (read priority)) (read timestamp) (unwords rest)
  _ -> Unknown "invalid"

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert message@(LogMessage _ messageTimestamp _) (Node left currentMessage@(LogMessage _ nodeTimestamp _) right)
  | messageTimestamp < nodeTimestamp = Node (insert message left) currentMessage right
  | otherwise = Node left currentMessage (insert message left)
insert _ _ = Leaf
