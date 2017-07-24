module MyUtil where

-- splitStacks eg. [1..8] = [[1],[2,3],[4,5,6],[7,8]]
splitStacks :: [a] -> [[a]]
splitStacks xs = splitStacks' 1 xs
  where splitStacks' _ [] = []
        splitStacks' n xs = list : (splitStacks' (n+1) rest)
          where
            (list,rest) = splitAt n xs
