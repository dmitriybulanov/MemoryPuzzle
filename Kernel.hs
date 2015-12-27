module Kernel where

import Data.Matrix
import Data.List

import System.Random
import System.Environment
import System.IO

import Control.Monad(when)

type Position = (Int, Int)
type FieldSize = Int
type FieldElem = Int

getRandomPosition :: StdGen -> FieldSize -> (Position, StdGen)
getRandomPosition gen size = let (i1, nGen) = (randomR (1,size) gen) in ((i1, fst $ randomR (1,size) nGen), 
                                    snd $ randomR (1,size) nGen)

genMatrixPositions :: StdGen -> FieldSize -> [Position]
genMatrixPositions generator size = foldl(\acc x -> mainFun acc x generator) [] [1..size^2]
    where
        mainFun used cur curGen = let (pos, nGen) = getRandomPosition curGen size in if elem pos used then
                mainFun used cur nGen else used ++ [pos]

fillMatrix :: Matrix Int -> [Position] -> Matrix FieldElem
fillMatrix = help 1
    where   
        help _ matr [] = matr
        help cur matr (x:y:xs) = help (cur + 1) (setElem cur y (setElem cur x matr)) xs 
 
generateFieldMatrix :: StdGen -> FieldSize -> Matrix FieldElem
generateFieldMatrix gen size = fillMatrix (zero size size) (genMatrixPositions gen size) 
 
isAllFounded :: Matrix FieldElem -> Bool
isAllFounded = null . dropWhile(== -1) . toList 

makeMove :: (Position, Position) -> Matrix FieldElem -> Matrix FieldElem
makeMove (pos1, pos2) matr = if check pos1 pos2 matr then guess pos1 pos2 matr else  matr
    where
        check (i1, j1) (i2, j2) matr = getElem i1 j1 matr == getElem i2 j2 matr && getElem i2 j2 matr /= -1
        guess pos1 pos2 matr = setElem (-1) pos1 (setElem (-1) pos2 matr)