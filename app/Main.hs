{-|
Module      : Main
Description : Main game executable. 

Invoke by 

stack exec wolf -- n

where n is initial position of the wolf, a positive integral from 1 to 4. Integral denotes position of the wolf on the first (upper) row, from left to right. 

-}

module Main where

import Board
import Board.Coordinate
import Control.Concurrent
import Data.Maybe
import System.Console.ANSI
import System.Environment

wolfWon :: IO ()
wolfWon = putStrLn "   GAME OVER! WOLF WON\n"

sheepsWon :: IO ()
sheepsWon = putStrLn "   GAME OVER! SHEEPS WON\n"

gameLoop :: Board.Board -> IO ()
gameLoop b = do
    toFile b "game.was"
    let boardLines = 1 + length (lines (show b))
    let sheepMove = bestSheepMove 0 b
    if isNothing sheepMove then wolfWon else do
        let afterSheepMove = b >>> fst (fromJust sheepMove)
        cursorUp boardLines
        print afterSheepMove
        threadDelay 500000
        let wolfMove = bestWolfMove 0 afterSheepMove
        if isNothing wolfMove then sheepsWon else do
            let afterWolfMove = afterSheepMove >>> fst (fromJust wolfMove)
            cursorUp boardLines
            print afterWolfMove 
            threadDelay 500000
            if wolfCoord afterWolfMove `elem` [B8, D8, F8, H8] then wolfWon
            else gameLoop afterWolfMove


main :: IO ()
main = do
    args <- getArgs
    let wolfPos = read (head args) :: Int
    let b = Board.init wolfPos
    print b
    threadDelay 1000000
    gameLoop b


