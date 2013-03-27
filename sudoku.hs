import Data.Char
import Data.List
import System.Environment
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    validArg <- parseArgs args
    let board = makeBoard $ stringToInts validArg
    putStrLn $ printBoard board
    putStrLn "Solved:"
    putStrLn $ printBoard $ solve board

parseArgs :: [String] -> IO String
parseArgs args = case args of
    [s] -> if validateArg s then return s else do { putStrLn "Invalid arg";
                                                    exitWith (ExitFailure 1) }
    _   -> do { putStrLn "Invalid number of args";
                exitWith (ExitFailure 1) }

validateArg :: String -> Bool
validateArg arg = length arg == 81 && all (\x -> x `elem` ['0'.. '9']) arg

stringToInts :: String -> [Int]
stringToInts = map digitToInt

solve :: Eq a => [[a]] -> [[a]]
solve orig_board = solve_help orig_board $ found orig_board
    where solve_help board [] = board
          solve_help board (x:xs) = solve_help (newboard 0 board) newfound
            where affected = affectedPos x
                  value = head (board !! x)
                  newfound = xs ++ [y | y <- affected, length (board !! y) == 2
                                        , value `elem` (board !! y)]
                  newboard _ [] = []
                  newboard pos (y:ys) = (if pos `elem` affected
                                            then delete value y else y)
                                         : newboard (pos + 1) ys

found :: Num a => [[a1]] -> [a]
found board = found_help board 0
    where found_help [] _ = []
          found_help (x:xs) pos
            | length x == 1 = pos : found_help xs (pos + 1)
            | otherwise = found_help xs (pos + 1)

-- Turn a list of numbers, with 0 representing unknown, into sudoku board
makeBoard :: (Enum t, Eq t, Num t) => [t] -> [[t]]
makeBoard = map (\x -> if x == 0 then [1.. 9] else [x])

-- Print pretty presentation of board to stdout. O represent unknown value.
printBoard :: Show a => [[a]] -> String
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
                  | n `mod` 27 == 0 && n /= 0 = "---------------------\n"
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
