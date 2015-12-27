module Kernel where

import Data.Matrix
import Data.List

import System.Random
import System.Environment
import System.IO

import Control.Monad(when)

type Position = (Int, Int)

data FieldElem a = Opened a | Closed a | Founded
    deriving (Show, Eq)

instance Functor FieldElem where
    fmap f (Opened num) = Opened (f num)
    fmap f (Closed num) = Closed (f num)
    fmap f _ = Founded

-- закрывает карту, если та открыта    
setClosed :: Position -> Matrix (FieldElem Int) -> Matrix (FieldElem Int)
setClosed pos@(i1, j1) matr = case getElem i1 j1 matr of 
        (Opened el) -> setElem (Closed el) pos matr
        (Closed el) -> matr
        Founded -> matr

-- генерация рандомной позиции в матрице        
getRandomPosition :: StdGen -> Int -> Int -> (Position, StdGen)
getRandomPosition gen sizeC sizeR = let (i1, nGen) = (randomR (1,sizeC) gen) in ((i1, fst $ randomR (1,sizeR) nGen), 
                                    snd $ randomR (1,sizeR) nGen)

-- генерация списка позиций в матрице, две подряд идущие будут хранить одно число                                    
genMatrixPositions :: StdGen -> Int -> Int -> [Position]
genMatrixPositions generator sizeC sizeR = foldl(\acc x -> mainFun acc x generator) [] [1..sizeC*sizeR]
    where
        mainFun used cur curGen = let (pos, nGen) = getRandomPosition curGen sizeC sizeR in if elem pos used then
                mainFun used cur nGen else used ++ [pos]

-- заполнение матрице в соответствии со сгенерированными позициями
fillMatrix :: Matrix (FieldElem Int) -> [Position] -> Matrix (FieldElem Int)
fillMatrix = help 1
    where   
        help _ matr [] = matr
        help cur matr (x:y:xs) = help (cur + 1) (setElem (Closed cur) y (setElem (Closed cur) x matr)) xs 

-- генерация матрицы со случайным расположением пары элементов        
generateFieldMatrix :: StdGen -> Int -> Int -> Matrix (FieldElem Int)
generateFieldMatrix gen sizeC sizeR = fillMatrix (myZero sizeC sizeR) (genMatrixPositions gen sizeC sizeR) 

-- генерация нулевой матрицы
myZero :: Int -> Int -> Matrix (FieldElem Int)
myZero colums rows = fromLists $ replicate colums (replicate rows Founded) 

-- все ли угаданы?  
isAllFounded :: Matrix (FieldElem Int) -> Bool
isAllFounded = null . dropWhile(== Founded) . toList 

-- открытие карты
openCard :: Position -> Matrix (FieldElem Int)-> Matrix (FieldElem Int)
openCard (i, j) matr = case getElem i j matr of
    (Closed el) -> setElem (Opened el) (i, j) matr
    _ -> matr
 
-- проверка двух открытых карт 
checkOpened :: (Position, Position) -> Int -> Matrix (FieldElem Int) -> (Matrix (FieldElem Int), Bool)
checkOpened (pos1, pos2) num matr = if check pos1 pos2 matr then guess pos1 pos2 matr else negative pos1 pos2 matr
    where
        inverseMod num1 num2 = mod num2 num1   
        check (i1, j1) (i2, j2) matr = fmap (inverseMod num) (getElem i1 j1 matr) == fmap (inverseMod num) (getElem i2 j2 matr) && getElem i2 j2 matr /= Founded
        guess pos1 pos2 matr = (setElem Founded pos1 (setElem Founded pos2 matr), True)
        negative pos1 pos2 matr = (setClosed pos1 (setClosed pos2 matr), False)
        