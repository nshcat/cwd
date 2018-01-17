module Utility
    ( replaceElement
    , clamp
    ) where


-- | Replace nth element in list with new value
replaceElement :: [a] -> Int -> a -> [a]
replaceElement xs i x = fore ++ (x : aft)
  where fore = take i xs
        aft  = drop (i+1) xs
        
-- | Clamp a value to given interval        
clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx
                 


