import Data.List

nearly_solved = [
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
main = putStr $ (print_board $ make_board nearly_solved)
                ++ "\nSolved:\n" ++ (print_board $ solve nearly_solved)

solve orig = solve_help $ make_board orig
    where solve_help board
           | is_solved board = board
           | otherwise = solve_help $ update board 0

is_solved board = all (\x -> length x == 1) board

make_board :: (Enum t, Eq t, Num t) => [t] -> [[t]]
make_board [] = []
make_board (x:xs) = [if x == 0 then [1.. 9] else [x]] ++ make_board xs

update :: Eq a => [[a]] -> Int -> [[a]]
update board n
    | length board == n = board
    | length (board !! n) /= 1 = update board (n + 1)
    | otherwise = update new_board (n + 1)
        where value = board !! n !! 0
              new_board = [if x `elem` affected_pos n then rem_elem value $ board !! x
                                                      else board !! x | x <- [0.. 80]]

print_board board = pri_help 0
    where pri_help n
           | length board == n = ""
           | otherwise = linesep ++ symbol ++ sep ++ pri_help (n + 1)
           where symbol = if length (board !! n) == 1
                          then show $ head (board !! n) else "X"
                 sep = if (n + 1) `mod` 9 == 0 then "\n" else if (n + 1) `mod` 3 == 0 then " | " else " "
                 linesep = if n == 27 || n == 54 then "---------------------\n" else ""

rem_elem :: Eq t => t -> [t] -> [t]
rem_elem value list = [x | x <- list, x /= value]

affected_pos :: Integral t => t -> [t]
affected_pos n = rem_elem n (nub $ line_pos n ++ column_pos n ++ field_pos n)

line_pos :: Integral t => t -> [t]
line_pos n = [base .. base + 8]
    where base = n `div` 9 * 9

column_pos :: Integral t => t -> [t]
column_pos n  = [n, n + 9.. 80]

field_pos :: Integral t => t -> [t]
field_pos n = [x + row_field * 27 + column_field * 3 | x <- pos_in_first_field]
    where pos_in_first_field = [0, 1, 2, 9, 10, 11, 18, 19, 20]
          column_field = (n - (9 * (n `div` 9))) `div` 3
          row_field = n `div` 27
