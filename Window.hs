import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Kernel
import System.Random
import Data.Matrix

-- Константы 
-- _______________________________________________________________________________________


width, height, offset :: Int
width = 1024
height = 768
offset = 0

startX = -280
startY = 150
intervalX = 15
intervalY = 15
sizeX = 80
sizeY = 100
count = 8
countC = 4
countR = 8
countOfPicks = 5
background :: Color
background = white

fps :: Int
fps = 3

type WindowPosition = (Float, Float)

-- _______________________________________________________________________________________




data Mode = Easy | Medium | Hard | NotSelected
    deriving (Show, Eq)

data GameStatus = ModeSelection | GameStarted | MainMenu | GamePaused | GameFinished | GameExit | ChekingCard
    deriving (Show, Eq)



data MemoryPuzzleGame = Game
    {
        field :: Matrix (FieldElem Int)
    ,   rectPositions :: Matrix WindowPosition
    ,   firstSelectedCard  :: Position
    ,   secondSelectedCard :: Position
    ,   timer :: Float
    ,   score :: Int
    ,   difficult :: Mode
    ,   status :: GameStatus
    ,   using :: Float
    }   
    deriving Show


getStatus :: MemoryPuzzleGame -> GameStatus
getStatus (Game _ _ _ _ _ _ _ currentStatus _) = currentStatus

isUsing :: MemoryPuzzleGame -> Bool
isUsing (Game _ _ _ _ _ _ _ _ uses) = if(uses == 0) then False else True


    
initialNewGame :: StdGen -> MemoryPuzzleGame
initialNewGame gen = Game   
    {
        field = generateFieldMatrix gen countC countR
    ,   rectPositions = fromLists []
    ,   firstSelectedCard = (-1,-1)
    ,   secondSelectedCard = (-1,-1)
    ,   timer = 0.0
    ,   score = 0
    ,   difficult = NotSelected
    ,   status = MainMenu
    ,   using = 0
    }   

 
 
 -- Функции для работы с картами
-- ____________________________________________________________________________________________________________________________


-- генерация позиций иконок начиная со стартовой точки
genRectPositions :: Float -> Float -> Int -> Int -> Float -> Float -> Float -> Float -> Matrix WindowPosition
genRectPositions stX stY countC countR szX szY intervalX intervalY = fromLists $ fst $ foldl(foldFunI) ([], (stX, stY)) [1..countC]
    where
        genArrayOfRect stX stY = fst $ foldl(foldFunJ) ([], (stX, stY)) [1..countR]
        foldFunJ (list, (stX, stY)) x = (list ++ [(stX, stY)], (stX + intervalX + sizeX, stY))
        foldFunI (list, (stX, stY)) x = (list ++ [genArrayOfRect stX stY]
            , (stX, stY - intervalY - sizeY))


genRectagles :: Matrix (FieldElem Int) -> Matrix WindowPosition -> [Picture] -> [Picture]
genRectagles elems positions picks = fst $ foldl(foldFunI) ([], 0) (toList positions)
    where
        getTranslate stX stY curInd = translate stX stY $ getPick curInd
        foldFunI (list, count) (x, y) = (list ++ [getTranslate x y count], count + 1)
        getPick curInd = case (toList elems) !! curInd of
            (Opened elem) -> picks !! (mod elem countOfPicks)
            (Closed elem) -> picks !! (length picks - 1)
            Deleted -> rectangleSolid 50 50


loadIcons :: Int -> IO [Picture]
loadIcons count = do
    addIcon (foldl (\acc x -> addIcon acc ("icons\\" ++ show x ++ ".bmp")) (loadIcon "icons\\1.bmp") [2..count]) ("icons\\suit.bmp")
        where
            loadIcon :: String -> IO [Picture]
            loadIcon fname = do
                bmp <- loadBMP fname
                return $ [bmp]
            addIcon :: IO [Picture] -> String -> IO [Picture]
            addIcon picks line = (++) <$> picks <*> (loadIcon line)


findIndexInMatr :: [WindowPosition] -> WindowPosition -> Position
findIndexInMatr positions clickPos = let (indI, indJ, (resI, resJ)) = foldl(foldlI) (0, 0, (-1,-1)) positions in if resI == -1 then (-1, -1) else (resI + 1, resJ + 1)
        where 
            isNear (x1, y1) (x2, y2) = abs (x1 - x2) < sizeX / 2 && abs (y1 - y2) < sizeY / 2
            foldlI (i, j, (resI, resJ)) curPos = if isNear curPos clickPos then (calcI, mod (j + 1) countR, (i, j)) else (calcI, mod (j + 1) countR, (resI, resJ))
                where   
                    calcI = if mod (j + 1) countR == 0 then mod (i+1) countC else i

-- ____________________________________________________________________________________________________________________________




render :: [Picture] -> Picture -> Picture -> Picture -> MemoryPuzzleGame -> Picture
render _ p1 _ _ (Game _ _ _ _ _ _ _ MainMenu _) =  p1
render _ _ p2 _ (Game _ _ _ _ _ _ _ ModeSelection _)  = p2
render picks _ _ p3 (Game field rectPositions _ _ _ _ _ GameStarted _) = pictures $ [p3] ++ (genRectagles field rectPositions picks)
render picks _ _ p3 (Game field rectPositions _ _ _ _ _ ChekingCard _) = pictures $ [p3] ++ (genRectagles field rectPositions picks)


mWindow :: Display
mWindow = InWindow "Memory Puszzle" (width, height) (offset, offset)  
    
update :: Float -> MemoryPuzzleGame -> MemoryPuzzleGame
update _ (Game field rPositions fScard sScard time sc mode ChekingCard 0) = Game
            { 
                field = fst $ checkOpened (fScard,sScard) countOfPicks field
            ,   rectPositions = rPositions
            ,   firstSelectedCard = (-1,-1)
            ,   secondSelectedCard = (-1,-1)
            ,   timer = time
            ,   score = sc
            ,   difficult = mode
            ,   status = GameStarted
            ,   using = 0
            }
update _ (Game field rPositions fScard sScard time sc mode stat uses)  = Game
            { 
                field = field
            ,   rectPositions = rPositions
            ,   firstSelectedCard = fScard
            ,   secondSelectedCard = sScard
            ,   timer = time
            ,   score = sc
            ,   difficult = mode
            ,   status = stat
            ,   using = if (uses > 0 ) then uses-1 else 0
            }
