module Stats (Stats,
              initialStats,
              levelInStats,
              linesInStats,
              scoreInStats,
              addLinesToStats) where

data Stats = Stats { linesCount :: Int, score :: Int }

initialStats :: Stats
initialStats = Stats 0 0

levelInStats :: Stats -> Int
levelInStats stats = div (linesCount stats) 5

linesInStats :: Stats -> Int
linesInStats = linesCount

scoreInStats :: Stats -> Int
scoreInStats = score

addLinesToStats :: Int -> Stats -> Stats
addLinesToStats n (Stats l s) = Stats (l + n) (s + scoreForLines n)
    where scoreForLines :: Int -> Int
          scoreForLines 0 = 0
          scoreForLines 1 = 100
          scoreForLines 2 = 200
          scoreForLines 3 = 400
          scoreForLines 4 = 800
          scoreForLines _ = error "Wrong number of lines"