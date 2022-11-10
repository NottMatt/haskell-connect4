{- Connect 4 Game - mtiemersma19@georgefox.edu -}

import Text.Printf (printf)
import Data.List
import Data.Char
import Data.Time.Clock.POSIX
import Text.Read (readMaybe)
import System.IO

{- MAIN FUNCTION -}
main :: IO ()
main = do
    putStrLn "\nCONNECT 4 GAME\n"
    putStrLn (displayBoard emptyBoard)                          -- show new empty board
    gameLoop emptyBoard emptyRoom 'H'                           -- enter new game loop

type Board = [[Char]]                                           -- define Board type
type Room = [Int]                                               -- define Room type
emptyBoard = ["       ", "       ", "       ",
                 "       ", "       ", "       "]               -- template for new board
emptyRoom = [6, 6, 6, 6, 6, 6, 6]                               -- template for new room


{- GAME LOOP -}
gameLoop :: Board -> Room -> Char -> IO ()
gameLoop board room agent = do
    if agent == 'H' then do                                     -- if human turn...
        putStrLn "Choose a rank (1-7): "                        -- prompt for input
        numIn <- getLine

        if (numIn == "") || (not (all isDigit numIn)) then do
            putStrLn "Must enter number..."                     -- bad input
            gameLoop board room 'H'
        else do
            let choiceHuman = ((read numIn :: Int) - 1) `mod` 7 
            -- input always wraps (mod 7)
            
            let newBoard = makeMove board room choiceHuman 'H'  -- apply move to board

            if board == newBoard then do
                putStrLn "Rank is full..."                      -- bad rank
                gameLoop board room 'H'
            else do
                putStrLn (displayBoard newBoard)                -- show updated board
                if isWinner newBoard then do                    -- if winner board
                    putStrLn "You Win!"                         -- Human wins
                    putStr "\nPlay Again? (Y/N): "              -- prompt to play again
                    hFlush stdout
                    again <- getLine
                    if again == "Y" then do
                        putStrLn (displayBoard emptyBoard)
                        gameLoop emptyBoard emptyRoom 'H'
                    else putStrLn "Good Bye!"                   -- exit game

                else do                                         -- if not winner board
                    let newRoom = put choiceHuman ((room!!choiceHuman) - 1) room
                    gameLoop newBoard newRoom 'R'               -- continue game loop for Robot turn
    else do
        ms <- round `fmap` getPOSIXTime                         -- sample time for random move

        let choiceRobot = ms `mod` 7

        let newBoard = makeMove board room choiceRobot 'R'      -- apply move to board

        if board == newBoard then gameLoop board room 'R'       -- bad rank
        
        else do
            putStrLn "Robot Choosing..."
            putStrLn (displayBoard newBoard)                    -- show updated board
            if isWinner newBoard then do                        -- if winner board
                putStrLn "Robot Wins!"                          -- Robot wins
                putStr "\nPlay Again? (Y/N): "                  -- prompt to play again
                hFlush stdout
                again <- getLine
                if again == "Y" then do
                    putStrLn (displayBoard emptyBoard)
                    gameLoop emptyBoard emptyRoom 'H'
                else putStrLn "Good Bye!"                       -- exit game
            else do
                let newRoom = put choiceRobot ((room!!choiceRobot) - 1) room
                gameLoop newBoard newRoom 'H'                   -- continue game loop for Human turn

                
    

{- APPLY A MOVE TO BOARD AT GIVEN RANK -}
makeMove :: Board -> Room -> Int -> Char -> Board
makeMove board room rank agent
    | (nextAvailable rank board room) == -1 = board                         -- (-1) if no available room in rank
    | agent == 'H' = put2d rank (nextAvailable rank board room) 'O' board   -- places 'O' on board in next available spot
    | agent == 'R' = put2d rank (nextAvailable rank board room) '0' board   -- places '0' on board in next available spot

{- GET NEXT AVAILABLE POSITION IN GIVEN RANK -}
nextAvailable :: Int -> Board -> Room -> Int
nextAvailable rank board room
    | room!!rank > 0 = (6 - (room!!rank))       -- uses room to know how much room is left in rank
    | otherwise = -1

{- DISPLAY BOARD WITH FORMATTING -}
displayBoard :: Board -> String                 -- assumes terminal supports unicode
displayBoard board = "\n" ++ (unlines (map (\row -> "\t\x2551" ++ row ++ "\x2551") (reverse board))) 
    ++ "\t\x2560\x2550\x2550\x2550\x2550\x2550\x2550\x2550\x2563\n" 
    ++ "\t\x2551" ++ "1234567" ++ "\x2551\n"


{- CHECK IF BOARD IS WINNER -}
isWinner :: Board -> Bool
isWinner board
    | (True `elem` (map (\row -> isInfixOf "0000" row)) board) = True               -- robot win horizontally
    | (True `elem` (map (\row -> isInfixOf "OOOO" row)) board) = True               -- human win horizontally
    | (True `elem` (map (\row -> isInfixOf "0000" row)) (transpose board)) = True   -- robot win vertically
    | (True `elem` (map (\row -> isInfixOf "OOOO" row)) (transpose board)) = True   -- human win vertically
    | diagOnBoard board = True                                                      -- right diagonal
    | diagOnBoard (map (\row -> reverse row) board) = True                          -- left diagonal
    | otherwise = False
    

{- AGGREGATES DIAGONALS AND CHECKS FOR WIN -}
diagOnBoard :: Board -> Bool    -- I know this is an awful way to aggregate diagonals, don't worry about it...
diagOnBoard board = do          -- basically there are only 12 possible ways a run of 4 can fit on a 6x7 grid in one direction
    let r00 = [(get2d 0 0 board)] ++ [(get2d 1 1 board)] ++ [(get2d 2 2 board)] ++ [(get2d 3 3 board)] ++ " "
    let r01 = r00 ++ [(get2d 0 1 board)] ++ [(get2d 1 2 board)] ++ [(get2d 2 3 board)] ++ [(get2d 3 4 board)] ++ " "
    let r02 = r01 ++ [(get2d 0 2 board)] ++ [(get2d 1 3 board)] ++ [(get2d 2 4 board)] ++ [(get2d 3 5 board)] ++ " "
    let r10 = r02 ++ [(get2d 1 0 board)] ++ [(get2d 2 1 board)] ++ [(get2d 3 2 board)] ++ [(get2d 4 3 board)] ++ " "
    let r11 = r10 ++ [(get2d 1 1 board)] ++ [(get2d 2 2 board)] ++ [(get2d 3 3 board)] ++ [(get2d 4 4 board)] ++ " "
    let r12 = r11 ++ [(get2d 1 2 board)] ++ [(get2d 2 3 board)] ++ [(get2d 3 4 board)] ++ [(get2d 4 5 board)] ++ " "
    let r20 = r12 ++ [(get2d 2 0 board)] ++ [(get2d 3 1 board)] ++ [(get2d 4 2 board)] ++ [(get2d 5 3 board)] ++ " "
    let r21 = r20 ++ [(get2d 2 1 board)] ++ [(get2d 3 2 board)] ++ [(get2d 4 3 board)] ++ [(get2d 5 4 board)] ++ " "
    let r22 = r21 ++ [(get2d 2 2 board)] ++ [(get2d 3 3 board)] ++ [(get2d 4 4 board)] ++ [(get2d 5 5 board)] ++ " "
    let r30 = r22 ++ [(get2d 3 0 board)] ++ [(get2d 4 1 board)] ++ [(get2d 5 2 board)] ++ [(get2d 6 3 board)] ++ " "
    let r31 = r30 ++ [(get2d 3 1 board)] ++ [(get2d 4 2 board)] ++ [(get2d 5 3 board)] ++ [(get2d 6 4 board)] ++ " "
    let r32 = r31 ++ [(get2d 3 2 board)] ++ [(get2d 4 3 board)] ++ [(get2d 5 4 board)] ++ [(get2d 6 5 board)] ++ " "
    (isInfixOf "0000" r32) || (isInfixOf "OOOO" r32)

{- PLACES ELEMENT IN LIST AT GIVEN POSITION -}
put :: Int -> a -> [a] -> [a]
put pos newVal list = take pos list ++ newVal : drop (pos+1) list

{- PLACES ELEMENT IN LIST AT GIVEN COORDINATE -}
put2d :: Int -> Int -> a -> [[a]] -> [[a]]
put2d x y newVal mat = put y (put x newVal (mat!!y)) mat

{- GETS ELEMENT IN LIST AT GIVEN COORDINATE -}
get2d :: Int -> Int -> [[a]] -> a
get2d x y mat = (mat!!y)!!x