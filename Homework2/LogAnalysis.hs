{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

    
parseMessage :: String -> LogMessage
parseMessage ss
    | log_kind == "E" = LogMessage (Error error_priority) error_line error_info
    | log_kind == "I" = LogMessage Info general_line general_info
    | log_kind == "W" = LogMessage Warning general_line general_info
    | otherwise = Unknown ss
    where
        spilt_ss = words $ ss
        log_kind = head $ spilt_ss
        otherinfo = tail spilt_ss
        general_line = read . head $ otherinfo
        general_info = unwords . tail $ otherinfo
        error_priority = read . head $ otherinfo
        error_line = read . head . tail $ otherinfo
        error_info = unwords . tail . tail $ otherinfo

         
        
        
parse :: String -> [LogMessage]
parse ss = map parseMessage . lines $ ss


-------------------------------------------------------------
--insert ------
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t 
insert d Leaf  = Node Leaf d Leaf
insert d (Node lst n rst) = if root_time < insert_time then (Node lst n (insert d rst)) else (Node (insert d lst) n rst)
    where
        getTime :: LogMessage -> TimeStamp
        getTime x = case x of
            (LogMessage (Error _) t _ ) -> t
            (LogMessage Info t _ ) -> t
            (LogMessage Warning t _ ) -> t
            (Unknown _) -> 0
        root_time = getTime n
        insert_time = getTime d  



build :: [LogMessage] -> MessageTree
build ss = foldl (\a b -> insert b a) Leaf ss


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lst n rst) = (inOrder lst) ++ [n] ++ (inOrder rst)


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map (\(LogMessage _ _ info) -> info) ( filter go (inOrder . build $ messages))
            where
                go :: LogMessage -> Bool
                go ss = case ss of
                    (LogMessage (Error severity) _ _) -> if severity > 50 then True else False
                    _ -> False