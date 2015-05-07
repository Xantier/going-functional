-- File should be named LogAnalysis.hs

{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
	
-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage s = case msgstr of 
	("I":ts:msg) -> LogMessage Info (read ts :: Int) (unwords msg)
	("W":ts:msg) -> LogMessage Warning (read ts :: Int) (unwords msg)
	("E":eVal:ts:msg) -> LogMessage (Error (read eVal :: Int)) (read ts :: Int) (unwords msg)
	msg@_ -> Unknown (unwords msg)
	where msgstr = words s

parse :: String -> [LogMessage]
parse str = map parseMessage (lines str)

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg@(LogMessage _ _ _) (Leaf) = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _) (Node left curMsg@(LogMessage _ curTs _) right) 
	| ts < curTs = Node left curMsg (insert msg right)
	| ts >= curTs = Node (insert msg left) curMsg right
insert (LogMessage _ _ _) tree = tree

-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder (Leaf) = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

-- Exercise 5
extractString :: LogMessage -> String
extractString (Unknown msg) = msg
extractString (LogMessage _ _ msg) = msg

filterSev :: LogMessage -> Bool
filterSev (LogMessage (Error sev) _ _) = sev > 49
filterSev _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong xs = map extractString (filter filterSev (inOrder (build xs)))
