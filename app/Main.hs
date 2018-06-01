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
import Board.Coordinate
import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.Console.ANSI
import System.Console.GetOpt
import System.Environment
import System.Exit

-- | Delay between rounds, in microseconds
delay :: Int
delay = 330000

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

-- | Wolf winning message.
wolfWon :: IO ()
wolfWon = putStrLn "   GAME OVER! WOLF WON\n"

-- | Sheep winning message. 
sheepsWon :: IO ()
sheepsWon = putStrLn "   GAME OVER! SHEEPS WON\n"

-- | Executes game rounds in a loop. 
gameLoop :: Bool        -- ^ If true, game if auto-saved every round. 
         -> String      -- ^ Auto-save file name. 
         -> Board.Board -- ^ Current board state. 
         -> IO ()
gameLoop autosave fileName b = do
    when autosave $ Board.toFile b fileName
    -- let sheepMove = bestSheepMove 0 b
    let coords = sheepsCoords b
    print ( show ( sheepsCoords b) ++ "                                                                    ")
    userInput <- getLine 
    case userInput of
        "r" -> resetGame autosave fileName b
        "s" -> saveGame autosave fileName b
        "l" -> loadGame autosave fileName b
        "q" -> quitGame
        _ -> gameLoop' autosave fileName b  $ coords !! (read userInput :: Int)

resetGame autosave fileName x = do
    let b = Board.init 2
    let boardLines = 1 + length (lines (show b)) + 2
    cursorUp boardLines
    print b
    gameLoop autosave fileName $b

saveGame autosave fileName b = do
    Board.toFile b fileName
    let boardLines = 1 + length (lines (show b)) + 2
    cursorUp boardLines
    print b
    gameLoop autosave fileName $ b

loadGame autosave fileName x = do
    b <- Board.fromFile fileName
    let boardLines = 1 + length (lines (show b)) + 2
    cursorUp boardLines
    print b
    gameLoop autosave fileName $ Board.init 2

quitGame = print "GAME ENDED"

gameLoop' autosave fileName b coord = do
    let boardLines = 1 + length (lines (show b)) + 4
    let moves = filter (\ (Move c b) -> c == coord) $ validSheepsMoves b
    print moves
    moveId <- getLine
    let move = moves !! (read moveId :: Int)
    let sheepMove = Just (move, 0)
    if isNothing sheepMove then wolfWon else do
        let afterSheepMove = b >>> fst (fromJust sheepMove)
        cursorUp boardLines
        print afterSheepMove
        threadDelay delay
        let wolfMove = bestWolfMove 3 afterSheepMove
        if isNothing wolfMove then sheepsWon else do
            let afterWolfMove = afterSheepMove >>> fst (fromJust wolfMove)
            cursorUp boardLines
            print afterWolfMove 
            threadDelay delay
            if wolfCoord afterWolfMove `elem` [B8, D8, F8, H8] then wolfWon
            else gameLoop autosave fileName afterWolfMove 

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
        print b >> threadDelay delay
        gameLoop autoSave outputFile b
    else do
        let b = Board.init wolfPosition
        print b >> threadDelay delay
        gameLoop autoSave outputFile b