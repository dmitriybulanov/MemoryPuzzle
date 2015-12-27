module Kernel where

import Data.Matrix
import Data.List

import System.Random
import System.Environment
import System.IO

import Control.Monad(when)

type Position = (Int, Int)
type FieldSize = Int

data FieldElem a = Opened a | Closed a | Founded
    deriving (Show, Eq)

instance Functor FieldElem where
    fmap f (Opened num) = Opened (f num)
    fmap f (Closed num) = Closed (f num)
    fmap f _ = Founded
    
isOpened :: FieldElem Int -> Bool    
isOpened (Opened x) = True
isOpened _ = False

isFounded :: FieldElem Int -> Bool    
isFounded Founded = True
isFounded _ = False

isClosed :: FieldElem Int -> Bool    
isClosed (Closed x) = True
isClosed _ = False
 
setClosed :: Position -> Matrix (FieldElem Int) -> Matrix (FieldElem Int)
setClosed pos@(i1, j1) matr = case getElem i1 j1 matr of 
        (Opened el) -> setElem (Closed el) pos matr
        (Closed el) -> matr
        Founded -> matr
 
getRandomPosition :: StdGen -> Int -> Int -> (Position, StdGen)
getRandomPosition gen sizeC sizeR = let (i1, nGen) = (randomR (1,sizeC) gen) in ((i1, fst $ randomR (1,sizeR) nGen), 
                                    snd $ randomR (1,sizeR) nGen)

genMatrixPositions :: StdGen -> Int -> Int -> [Position]
genMatrixPositions generator sizeC sizeR = foldl(\acc x -> mainFun acc x generator) [] [1..sizeC*sizeR]
    where
        mainFun used cur curGen = let (pos, nGen) = getRandomPosition curGen sizeC sizeR in if elem pos used then
                mainFun used cur nGen else used ++ [pos]

fillMatrix :: Matrix (FieldElem Int) -> [Position] -> Matrix (FieldElem Int)
fillMatrix = help 1
    where   
        help :: Int -> Matrix (FieldElem Int) -> [Position] -> Matrix (FieldElem Int)
        help _ matr [] = matr
        help cur matr (x:y:xs) = help (cur + 1) (setElem (Closed cur) y (setElem (Closed cur) x matr)) xs 
 
generateFieldMatrix :: StdGen -> Int -> Int -> Matrix (FieldElem Int)
generateFieldMatrix gen sizeC sizeR = fillMatrix (myZero sizeC sizeR) (genMatrixPositions gen sizeC sizeR) 

myZero :: Int -> Int -> Matrix (FieldElem Int)
myZero colums rows = fromLists $ replicate colums (replicate rows Founded) 
  
isAllFounded :: Matrix (FieldElem Int) -> Bool
isAllFounded = null . dropWhile(== Founded) . toList 

openCard :: Position -> Matrix (FieldElem Int)-> Matrix (FieldElem Int)
openCard (i, j) matr 
    | isFounded $ getElem i j matr = matr
    | isOpened $ getElem i j matr = matr
    | otherwise = let (Closed elem) = getElem i j matr in (setElem (Opened elem) (i, j) matr)
 
checkOpened :: (Position, Position) -> Int -> Matrix (FieldElem Int) -> Matrix (FieldElem Int)
checkOpened (pos1, pos2) num matr = if check pos1 pos2 matr then guess pos1 pos2 matr else negative pos1 pos2 matr
    where
        check (i1, j1) (i2, j2) matr = fmap (mod num) (getElem i1 j1 matr) == fmap (mod num) (getElem i2 j2 matr) && getElem i2 j2 matr /= Founded
        guess pos1 pos2 matr = setElem Founded pos1 (setElem Founded pos2 matr)
        negative pos1 pos2 matr = setClosed pos1 (setClosed pos2 matr)
        