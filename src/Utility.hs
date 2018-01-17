module Utility
    ( replaceElement
    ) where


replaceElement :: [a] -> Int -> a -> [a]
replaceElement xs i x = fore ++ (x : aft)
  where fore = take i xs
        aft  = drop (i+1) xs
                 


