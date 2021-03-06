{-# LANGUAGE ViewPatterns, RankNTypes #-}

module Kernel where

import Data.Matrix
import Data.List

import System.Random
import System.Environment
import System.IO

import Control.Concurrent.MonadIO
import Control.Monad(when)

type Position = (Int, Int)
type GenAction m = forall a. Random a => m a
type GenActionR m = forall a. Random a => (a, a) -> m a

data FieldElem a = Opened a | Closed a | Deleted
    deriving (Show, Eq)

instance Functor FieldElem where
    fmap f (Opened num) = Opened (f num)
    fmap f (Closed num) = Closed (f num)
    fmap f _ = Deleted

-- применение какой-то функции ко всем картам
funToAll :: (FieldElem Int -> FieldElem Int) ->  Matrix (FieldElem Int) -> Matrix (FieldElem Int)
funToAll f matrix = fromList (nrows matrix) (ncols matrix) $ map f $ toList matrix

closeAll :: Matrix (FieldElem Int) -> Matrix (FieldElem Int)
closeAll = funToAll(\(Opened e) -> (Closed e))

openAll :: Matrix (FieldElem Int) -> Matrix (FieldElem Int)
openAll = funToAll(\(Closed e) -> (Opened e))

deleteAll :: Matrix (FieldElem Int) -> Matrix (FieldElem Int)
deleteAll = funToAll(\x -> Deleted)

-- генерация рандомных позиций в матрице        
getRandomPosition :: StdGen -> Int -> Int -> (Position, StdGen)
getRandomPosition gen sizeC sizeR = let (i1, nGen) = (randomR (1,sizeC) gen) in ((i1, fst $ randomR (1,sizeR) nGen), 
                                    snd $ randomR (1,sizeR) nGen)
                                  
genMatrixPositions :: StdGen -> Int -> Int -> [Position]
genMatrixPositions generator sizeC sizeR = foldl(mainFun generator) [] [1..sizeC*sizeR]
    where
        mainFun curGen used cur = let (pos, nGen) = getRandomPosition curGen sizeC sizeR in if elem pos used then
                mainFun nGen used cur else used ++ [pos]  

-- заполнение матрице в соответствии со сгенерированными позициями
fillMatrix :: Matrix (FieldElem Int) -> [Position] -> Matrix (FieldElem Int)
fillMatrix = help 1
    where   
        help _ matr [] = matr
        help cur matr (x:y:xs) = help (cur + 1) (setElem (Closed cur) y (setElem (Closed cur) x matr)) xs 

-- Random генерация позиции гибко с помощью расширения RankNTypes
randomPosition :: MonadIO m => GenActionR m -> Int -> Int -> m [Position]
randomPosition genR sizeC sizeR = do
    i1 <- genR (1, sizeC)
    i2 <- genR (1, sizeR)
    return $ [(i1, i2)]

genRandomPositions :: Int -> Int -> IO [Position]
genRandomPositions = help []
    where 
        help :: [Position] -> Int -> Int -> IO [Position]
        help list sizeC sizeR = do
            arr <- randomPosition randomRIO sizeC sizeR
            if (not $ length (nub list) == (sizeC * sizeR)) then (help (arr ++ (nub list)) sizeC sizeR) else return (nub list)

generateFieldMatrixWithRNT :: StdGen -> Int -> Int -> IO (Matrix (FieldElem Int))
generateFieldMatrixWithRNT gen sizeC sizeR = do
    positions <- genRandomPositions sizeC sizeR
    return $ fillMatrix (myZero sizeC sizeR) positions            
            

-- генерация матрицы со случайным расположением пары элементов        
generateFieldMatrix :: StdGen -> Int -> Int -> Matrix (FieldElem Int)
generateFieldMatrix gen sizeC sizeR = fillMatrix (myZero sizeC sizeR) (genMatrixPositions gen sizeC sizeR) 

-- генерация нулевой матрицы
myZero :: Int -> Int -> Matrix (FieldElem Int)
myZero colums rows = fromLists $ replicate colums (replicate rows Deleted) 

-- все ли угаданы?  
isAllFounded :: Matrix (FieldElem Int) -> Bool
isAllFounded = null . dropWhile(== Deleted) . toList 

-- открытие карты, если она открыта
openCard :: Position -> Matrix (FieldElem Int)-> Matrix (FieldElem Int)
openCard (i, j) matr@(getElem i j -> Closed el) = setElem (Opened el) (i, j) matr
openCard _ matr = matr
    
-- закрывает карту, если та открыта    
closeCard :: Position -> Matrix (FieldElem Int) -> Matrix (FieldElem Int)
closeCard (i, j) matr@(getElem i j -> Opened el) = setElem (Closed el) (i, j) matr
closeCard _ matr = matr

--  удаление карты, удалять можно, только если она открыта       
deleteCard :: Position -> Matrix (FieldElem Int) -> Matrix (FieldElem Int)
deleteCard (i, j) matr@(getElem i j -> Opened el) = setElem Deleted (i, j) matr
deleteCard _ matr = matr     
        
-- проверка двух открытых карт 
checkOpened :: (Position, Position) -> Int -> Matrix (FieldElem Int) -> (Matrix (FieldElem Int), Bool)
checkOpened (pos1@(i1,j1), pos2@(i2, j2)) num matr = if check then guess else negative
    where
        inverseMod num1 num2 = mod num2 num1   
        check = fmap (inverseMod num) (getElem i1 j1 matr) == fmap (inverseMod num) (getElem i2 j2 matr) && getElem i2 j2 matr /= Deleted
        guess = (deleteCard pos1 (deleteCard pos2 matr), True)
        negative = (closeCard pos1 (closeCard pos2 matr), False)