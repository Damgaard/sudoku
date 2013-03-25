import Data.List

nearlySolved = [
     2, 4, 8, 3, 9, 5, 7, 1, 6,
     0, 7, 1, 6, 2, 8, 3, 4, 9,
     9, 0, 6, 7, 4, 1, 5, 8, 2,
     6, 8, 2, 5, 3, 9, 1, 7, 4,
     0, 0, 9, 1, 7, 4, 6, 2, 8,
     7, 1, 4, 8, 6, 2, 9, 5, 3,
     8, 6, 3, 4, 1, 7, 2, 9, 5,
     1, 9, 5, 2, 8, 6, 4, 3, 7,
     4, 2, 7, 9, 5, 3, 8, 6, 1
     ]

main :: IO ()
main = putStr $ printBoard (makeBoard nearlySolved)
                ++ "\nSolved:\n" ++ printBoard (solve nearlySolved)

solve :: (Enum a, Eq a, Num a) => [a] -> [[a]]
solve orig = solve_help $ makeBoard orig
    where solve_help board
           | isSolved board = board
           | otherwise = solve_help $ update board 0

isSolved :: [[a]] -> Bool
isSolved = all (\x -> length x == 1)

-- Turn a list of numbers, with 0 representing unknown, into sudoku board
makeBoard :: (Enum t, Eq t, Num t) => [t] -> [[t]]
makeBoard = map (\x -> if x == 0 then [1.. 9] else [x])

update :: Eq a => [[a]] -> Int -> [[a]]
update board n
    | length board == n = board
    | length (board !! n) /= 1 = update board (n + 1)
    | otherwise = update new_board (n + 1)
        where value = head (board !! n)
              new_board = [if x `elem` affectedPos n
                            then delete value $ board !! x
                            else board !! x
                           | x <- [0.. 80]]

-- Print pretty presentation of board to stdout. O represent unknown value.
printBoard board = pri_help 0
    where pri_help n
           | length board == n = ""
           | otherwise = linesep ++ symbol ++ sep ++ pri_help (n + 1)
           where symbol = if length (board !! n) == 1
                          then show $ head (board !! n) else "0"
                 sep
                  | (n + 1) `mod` 9 == 0 = "\n"
                  | (n + 1) `mod` 3 == 0 = " | "
                  | otherwise = " "
                 linesep
                  | (n `mod` 27 == 0) && (n /= 0) = "---------------------\n"
                  | otherwise = ""

-- The pos that would be affected by a change at pos n
affectedPos :: Integral t => t -> [t]
affectedPos n = delete n $ nub $ linepos n ++ columnPos n ++ fieldPos n

linepos :: Integral t => t -> [t]
linepos n = [base.. base + 8]
    where base = n `div` 9 * 9

columnPos :: Integral t => t -> [t]
columnPos n  = [n, n + 9.. 80]

fieldPos :: Integral t => t -> [t]
fieldPos n = [x + row_field * 27 + column_field * 3 | x <- pos_in_first_field]
    where pos_in_first_field = [0, 1, 2, 9, 10, 11, 18, 19, 20]
          column_field = (n - (9 * (n `div` 9))) `div` 3
          row_field = n `div` 27
