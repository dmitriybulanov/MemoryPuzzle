module Kernel where

import Data.Matrix
import Data.List

import System.Random
import System.Environment
import System.IO

import Parser
import SimpleParsers
import ParseNumbers

import Control.Monad(when)

type Position = (Int, Int)
type FieldSize = Int

data FieldElem = Opened Int | Closed Int | Founded
    deriving (Show, Eq)

isOpened :: FieldElem -> Bool    
isOpened (Opened x) = True
isOpened _ = False

isFounded :: FieldElem -> Bool    
isFounded Founded = True
isFounded _ = False

isClosed :: FieldElem -> Bool    
isClosed (Closed x) = True
isClosed _ = False
            
getRandomPosition :: StdGen -> FieldSize -> (Position, StdGen)
getRandomPosition gen size = let (i1, nGen) = (randomR (1,size) gen) in ((i1, fst $ randomR (1,size) nGen), 
                                    snd $ randomR (1,size) nGen)

genMatrixPositions :: StdGen -> FieldSize -> [Position]
genMatrixPositions generator size = foldl(\acc x -> mainFun acc x generator) [] [1..size^2]
    where
        mainFun used cur curGen = let (pos, nGen) = getRandomPosition curGen size in if elem pos used then
                mainFun used cur nGen else used ++ [pos]

fillMatrix :: Matrix FieldElem -> [Position] -> Matrix FieldElem
fillMatrix = help 1
    where   
        help :: Int -> Matrix FieldElem -> [Position] -> Matrix FieldElem
        help _ matr [] = matr
        help cur matr (x:y:xs) = help (cur + 1) (setElem (Closed cur) y (setElem (Closed cur) x matr)) xs 
 
generateFieldMatrix :: StdGen -> FieldSize -> Matrix FieldElem
generateFieldMatrix gen size = fillMatrix (myZero size size) (genMatrixPositions gen size) 


myZero :: Int -> Int -> Matrix FieldElem
myZero colums rows = fromLists $ replicate colums (replicate rows Founded) 

  
isAllFounded :: Matrix FieldElem -> Bool
isAllFounded = null . dropWhile(== Founded) . toList 

openCard :: Position -> Matrix FieldElem -> Matrix FieldElem
openCard (i, j) matr 
    | getElem i j matr == Founded = matr
    | isOpened $ getElem i j matr = matr
    | otherwise = let (Closed elem) = getElem i j matr in (setElem (Opened elem) (i, j) matr)
        
checkOpened :: (Position, Position) -> Matrix FieldElem -> Matrix FieldElem
checkOpened (pos1, pos2) matr = if check pos1 pos2 matr then guess pos1 pos2 matr else  matr
    where
        check (i1, j1) (i2, j2) matr = getElem i1 j1 matr == getElem i2 j2 matr && getElem i2 j2 matr /= Founded
        guess pos1 pos2 matr = setElem Founded pos1 (setElem Founded pos2 matr)       