join :: String -> [String] -> String
join split [word] = word
join split (word:words) = word ++ split ++ join split words
join split [] = ""
  
  
