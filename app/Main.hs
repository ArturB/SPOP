{-|
Module      : Main
Description : Main game executable. 

Invoke by 

stack exec wolf -- n

where n is initial position of the wolf, a positive integral from 1 to 4. Integral denotes position of the wolf on the first (upper) row, from left to right. 

-}

module Main where

import Board
import Move.Direction
import Move
import Move.Status
import Board.Coordinate
import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.Console.ANSI
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Read

-- | Delay between rounds, in microseconds
delay :: Int
delay = 600000

-- | prompt length
promptLength :: Int
promptLength = 32

-- | Available command line options
data Option = WolfPosition String | InputFile String | AutoSave | OutputFile String | Help deriving (Eq, Read, Show)

-- | Available CLI options descriptors. 
opts :: [OptDescr Option]
opts = [
    Option "w" ["wolf-position"] (OptArg (WolfPosition . fromMaybe "3") "INT") "Initial position of the wolf. Must be integer from 1 to 4 and indicates wolf positon of the first row of the board, from left to right. Default 2",
    Option "i" ["input-file"]    (OptArg (InputFile . fromMaybe "") "FILENAME") "Read game from file",
    Option "s" ["auto-save"]     (NoArg AutoSave) "If set, the game is auto-saved every round. Default false",
    Option "o" ["output-file"]   (OptArg (OutputFile . fromMaybe "game.was") "FILENAME") "Name of file to auto-save the game, Default game.was",
    Option "h" ["help"]          (NoArg Help) "Show usage info"
    ]

-- | Using to split inputs.
split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

-- | Print gameboard as animated terminal. 
printGameboard :: Board.Board -- ^ Board to print
               -> Bool        -- ^ Clear previous board?
               -> String      -- ^ Optional message to print
               -> IO ()
printGameboard b cls msg = do
    let boardLines = 2 + length (lines (show b))
    when cls (cursorUp boardLines)
    print b
    putStrLn ("    sheep$ " ++ replicate promptLength ' ')
    putStr $ "\n    " ++ msg
    putStrLn $ replicate (promptLength - length msg) ' '
    putStrLn ""
    cursorUp 4
    cursorForward 11 >> hFlush stdout

-- | Run one game round. 
gameRound :: Bool      -- ^ If true, game if auto-saved every round. 
         -> String      -- ^ Auto-save file name. 
         -> Board.Board -- ^ Current board state. 
         -> IO ()
gameRound autosave fileName b = do
    when autosave $ Board.toFile b fileName
    -- get user input
    userInput <- getLine
    let inputs = split ' ' userInput
    if length inputs == 1 
    then case userInput of
        "r" -> do
                let bi = Board.init 2
                printGameboard bi True ""
                gameRound autosave fileName bi
        "q" -> putStrLn ""
        _   -> printGameboard b True "Unknown command!" >> gameRound autosave fileName b
    else if length inputs == 2 
        then case head inputs of
            "s" -> do
                    let fileToSave = inputs !! 1
                    Board.toFile b fileToSave
                    printGameboard b True ""
                    gameRound autosave fileName b
            "l" -> do
                    let fileToLoad = inputs !! 1
                    bl <- Board.fromFile fileToLoad
                    printGameboard bl True ""
                    gameRound autosave fileName bl
            _   -> do
                let userMove = parseUserMove inputs
                if isNothing userMove then printGameboard b True "Invalid move!" >> gameRound autosave fileName b
                else 
                    let status = Board.moveStatus b (fromJust userMove) 
                    in if status /= OK then printGameboard b True (show status) >> gameRound autosave fileName b else do
                        let b2 = b >>> fromJust userMove
                        printGameboard b2 True "" >> cursorDown 1 >> threadDelay delay 
                        let wolfMove = bestWolfMove 3 b2
                        if isNothing wolfMove then printGameboard b2 True "SHEEPS WON!" >> cursorDown 4 >> cursorBackward 11 else do 
                            let b3 = b2 >>> fst (fromJust wolfMove)
                            if wolfCoord b3 `elem` [B8, D8, F8, H8] then printGameboard b3 True "WOLF WON!" >> cursorDown 4 >> cursorBackward 11
                            else printGameboard b3 True "" >> gameRound autosave fileName b3

        else printGameboard b True "Invalid option" >> gameRound autosave fileName b

-- | Parses user move, checks if move is valid
parseUserMove :: [String] -> Maybe Move.Move
parseUserMove inputs = 
    let coord = readMaybe $ head inputs :: Maybe Coordinate
        dir = case inputs !! 1 of
            "L" -> Just UpLeft 
            "R" -> Just UpRight
            _   -> Nothing 
    in if isJust coord && isJust dir then Just (Move (fromJust coord) (fromJust dir)) else Nothing

-- | ENTRY POINT
main :: IO ()
main = do
    args <- getArgs
    let (options,nonOptions,errors) = getOpt RequireOrder opts args
    when (Help `elem` options) $ do
        putStrLn $ usageInfo "wolf.exe" opts
        exitSuccess
    let wolfPosition = foldl (\acc x -> case x of { (WolfPosition pos) -> read pos :: Int ; _ -> acc }) 2 options
    let inputFile = foldl (\acc x -> case x of { InputFile fileName -> fileName; _ -> acc}) "" options
    let autoSave = foldl (\acc x -> case x of { AutoSave -> True; _ -> acc}) False options
    let outputFile = foldl (\acc x -> case x of { OutputFile fileName -> fileName; _ -> acc}) "game.was" options
    if inputFile /= "" then do
        b <- Board.fromFile inputFile
        printGameboard b False ""
        gameRound autoSave outputFile b
    else do
        let b = Board.init wolfPosition
        printGameboard b False ""
        gameRound autoSave outputFile b

