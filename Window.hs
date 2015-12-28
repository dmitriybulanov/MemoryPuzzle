{-# LANGUAGE MultiWayIf, RecordWildCards #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Kernel
import System.Random
import Data.Matrix




-- Константы 
-- ____________________________________________________________________________________________________________________________



width, height, offset :: Int
width = 1024
height = 768
offset = 0

startX = -320
startY = 150
intervalX = 15
intervalY = 15
sizeX = 80
sizeY = 100

countOfPicks = 24

background :: Color
background = white

startTime = 300

fps :: Int
fps = 4

type WindowPosition = (Float, Float)
-- ____________________________________________________________________________________________________________________________





data Mode = Easy | Medium | Hard | NotSelected
    deriving (Show, Eq)

data GameStatus = ModeSelection | GameStarted | MainMenu | GamePaused | GameFinished | GameExit | ChekingCard | CardPreview
    deriving (Show, Eq)



data MemoryPuzzleGame = Game
    {
        field :: Matrix (FieldElem Int)
    ,   rectPositions :: Matrix WindowPosition
    ,   firstSelectedCard  :: Position
    ,   secondSelectedCard :: Position
    ,   timer :: Int
    ,   score :: Int
    ,   difficult :: Mode
    ,   status :: GameStatus
    ,   using :: Float
    }   
    deriving Show


getStatus :: MemoryPuzzleGame -> GameStatus
getStatus Game{..} = status

isUsing :: MemoryPuzzleGame -> Bool
isUsing Game{..} = if(using == 0) then False else True

getDifficult :: MemoryPuzzleGame -> Mode
getDifficult Game{..} = difficult

    
initialNewGame :: StdGen -> MemoryPuzzleGame
initialNewGame gen = Game   
    {
        field = generateFieldMatrix gen 2 2
    ,   rectPositions = fromLists []
    ,   firstSelectedCard = (-1,-1)
    ,   secondSelectedCard = (-1,-1)
    ,   timer = 0
    ,   score = 0
    ,   difficult = NotSelected
    ,   status = MainMenu
    ,   using = 2
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
            (Closed elem) -> picks !! (length picks - 2)
            Deleted -> picks !! (length picks - 1)


loadIcons :: Int -> IO [Picture]
loadIcons count = do
    addIcon (addIcon (foldl (\acc x -> addIcon acc ("icons\\" ++ show x ++ ".bmp")) (loadIcon "icons\\1.bmp") [2..count]) ("icons\\suit.bmp")) ("icons\\deleted.bmp")
        where
            loadIcon :: String -> IO [Picture]
            loadIcon fname = do
                bmp <- loadBMP fname
                return $ [bmp]
            addIcon :: IO [Picture] -> String -> IO [Picture]
            addIcon picks line = (++) <$> picks <*> (loadIcon line)


findIndexInMatr :: [WindowPosition] -> WindowPosition -> Int -> Int -> Position
findIndexInMatr positions clickPos countR countC = let (indI, indJ, (resI, resJ)) = foldl(foldlI) (0, 0, (-1,-1)) positions in if resI == -1 then (-1, -1) else (resI + 1, resJ + 1)
        where 
            isNear (x1, y1) (x2, y2) = abs (x1 - x2) < sizeX / 2 && abs (y1 - y2) < sizeY / 2
            foldlI (i, j, (resI, resJ)) curPos = if isNear curPos clickPos then (calcI, mod (j + 1) countR, (i, j)) else (calcI, mod (j + 1) countR, (resI, resJ))
                where   
                    calcI = if mod (j + 1) countR == 0 then mod (i+1) countC else i
-- ____________________________________________________________________________________________________________________________







-- Проверка на нажатие определенной кнопки
-- ____________________________________________________________________________________________________________________________


checkOnClickOnStart :: WindowPosition -> Bool 
checkOnClickOnStart (x,y) = if( x > -170 && x < 150) then if ( y > 10 && y < 140) then True else False  else False

checkOnClickOnExit :: WindowPosition -> Bool
checkOnClickOnExit (x,y) = if( x > -170 && x < 150) then if ( y > -150 && y < -20) then True else False  else False

checkOnClickOnBack :: WindowPosition -> Bool
checkOnClickOnBack (x,y) = if( x > -170 && x < 150) then if ( y > -330   && y < -210) then True else False  else False

checkOnClickOnEasyMode :: WindowPosition -> Bool
checkOnClickOnEasyMode (x,y) = if( x > -170 && x < 150) then if ( y > 120   && y < 240) then True else False  else False

checkOnClickOnMediumMode :: WindowPosition -> Bool
checkOnClickOnMediumMode (x,y) = if( x > -170 && x < 150) then if ( y > -10   && y < 110) then True else False  else False

checkOnClickOnHardMode :: WindowPosition -> Bool
checkOnClickOnHardMode (x,y) = if( x > -170 && x < 150) then if ( y > -150   && y < -30) then True else False  else False
  
checkOnClickOnPause :: WindowPosition -> Bool
checkOnClickOnPause (x,y) = if( x > 270 && x < 460) then if ( y >  270  && y < 400) then True else False  else False

checkOnClickOnContinue :: WindowPosition -> Bool
checkOnClickOnContinue (x,y) = if( x > -170 && x < 150) then if ( y > 0   && y < 110) then True else False  else False

checkOnClickOnMainMenu:: WindowPosition -> Bool
checkOnClickOnMainMenu (x,y) = if( x > -170 && x < 150) then if ( y > -150 && y < -20) then True else False  else False

checkOnClickOnMainMenuAfterRes:: WindowPosition -> Bool
checkOnClickOnMainMenuAfterRes (x,y) = if( x > -170 && x < 150) then if ( y > -260 && y < -120) then True else False  else False
-- ____________________________________________________________________________________________________________________________






-- Обработка нажатий мыши по форме
-- ____________________________________________________________________________________________________________________________

setNewStatus :: StdGen -> MemoryPuzzleGame -> GameStatus -> Mode -> MemoryPuzzleGame
setNewStatus gen Game{..} CardPreview newMode = Game
            {
                field = openAll $ generateFieldMatrix gen countC countR 
            ,   rectPositions = genRectPositions startX startY countC countR sizeX sizeY intervalX intervalY 
            ,   firstSelectedCard = firstSelectedCard
            ,   secondSelectedCard = secondSelectedCard
            ,   timer = timer
            ,   score = score
            ,   difficult = newMode
            ,   status = CardPreview
            ,   using = 5
            }
                where countC = if | newMode == Easy -> 2
                                  | newMode == Medium -> 4
                                  | otherwise -> 5
                      countR = 8
                      
setNewStatus _ Game{..} GamePaused _ = Game  
            {
                field = field
            ,   rectPositions = rectPositions
            ,   firstSelectedCard = firstSelectedCard
            ,   secondSelectedCard = secondSelectedCard
            ,   timer = timer
            ,   score = score
            ,   difficult = difficult
            ,   status = GamePaused
            ,   using = 2
            }
                     
setNewStatus gen _ MainMenu _ = initialNewGame gen
            
            
setNewStatus _ Game{..} newStatus _ = Game
            {
                field = field
            ,   rectPositions = rectPositions
            ,   firstSelectedCard = firstSelectedCard
            ,   secondSelectedCard = secondSelectedCard
            ,   timer = timer
            ,   score = score
            ,   difficult = difficult
            ,   status = newStatus
            ,   using = 2
            }
            



oCard :: Position -> MemoryPuzzleGame -> MemoryPuzzleGame
oCard pos (Game field rectPositions (-1,-1) (-1,-1) timer score difficult status _) = Game 
    {
        field = openCard  pos field
    ,   rectPositions = rectPositions
    ,   firstSelectedCard = pos
    ,   secondSelectedCard = (-1,-1)
    ,   timer = timer
    ,   score = score 
    ,   difficult = difficult
    ,   status = status
    ,   using = 2 
    }

oCard pos (Game field rectPositions firstSelectedCard (-1,-1) timer score difficult _ _) = Game 
    {
        field =  if (pos /= firstSelectedCard) then (openCard pos field) else field
    ,   rectPositions = rectPositions
    ,   firstSelectedCard = firstSelectedCard
    ,   secondSelectedCard = if (pos /= firstSelectedCard) then pos else (-1,-1)
    ,   timer = timer
    ,   score = score 
    ,   difficult = difficult
    ,   status = if (pos /= firstSelectedCard) then ChekingCard else GameStarted
    ,   using = 2
    }   

handleKeys :: StdGen ->  Event -> MemoryPuzzleGame -> MemoryPuzzleGame
handleKeys gen (EventKey (MouseButton LeftButton) _ _ position) currentGame
        | isUsing currentGame == False && getStatus currentGame  == MainMenu      && checkOnClickOnStart               position == True = setNewStatus gen currentGame ModeSelection NotSelected
        | isUsing currentGame == False && getStatus currentGame  == MainMenu      && checkOnClickOnExit                position == True = setNewStatus gen currentGame GameExit NotSelected
        | isUsing currentGame == False && getStatus currentGame  == ModeSelection && checkOnClickOnBack                position == True = setNewStatus gen currentGame MainMenu NotSelected
        | isUsing currentGame == False && getStatus currentGame  == ModeSelection && checkOnClickOnEasyMode            position == True = setNewStatus gen currentGame CardPreview Easy
        | isUsing currentGame == False && getStatus currentGame  == ModeSelection && checkOnClickOnMediumMode          position == True = setNewStatus gen currentGame CardPreview Medium
        | isUsing currentGame == False && getStatus currentGame  == ModeSelection && checkOnClickOnHardMode            position == True = setNewStatus gen currentGame CardPreview Hard
        | isUsing currentGame == False && getStatus currentGame  == GameStarted   && checkOnClickOnPause               position == True = setNewStatus gen currentGame GamePaused NotSelected   
        | isUsing currentGame == False && getStatus currentGame  == GamePaused    && checkOnClickOnContinue            position == True = setNewStatus gen currentGame GameStarted (getDifficult currentGame)
        | isUsing currentGame == False && getStatus currentGame  == GamePaused    && checkOnClickOnMainMenu            position == True = setNewStatus gen currentGame MainMenu NotSelected
        | isUsing currentGame == False && getStatus currentGame  == GameFinished  && checkOnClickOnMainMenuAfterRes    position == True = setNewStatus gen currentGame MainMenu NotSelected
        | isUsing currentGame == False && getStatus currentGame  == GameStarted   && cardPosition /= (-1,-1)                            = oCard cardPosition currentGame
        | otherwise = currentGame
                 where cardPosition = findIndexInMatr (toList $ rectPositions currentGame) position 8 $ if | getDifficult currentGame == Easy -> 2
                                                                                                           | getDifficult currentGame == Medium -> 4
                                                                                                           | otherwise -> 5
handleKeys _ _ currentGame = currentGame
-- ____________________________________________________________________________________________________________________________





-- Рендер формы в зависимости от состояния MemoryPuzzleGame
-- ____________________________________________________________________________________________________________________________


render :: [Picture] -> Picture -> Picture -> Picture -> Picture -> Picture -> MemoryPuzzleGame -> Picture
render _ p1 _ _ _ _ (Game _ _ _ _ _ _ _ MainMenu _) =  p1

render _ _ p2 _ _ _ (Game _ _ _ _ _ _ _ ModeSelection _)  = p2

render picks _ _ p3 _ _ (Game field rectPositions _ _ timer score _ GameStarted _) = pictures $ 
        [
           p3
        ,  Color white $ translate (-285) 280 $ Scale 0.6 0.6 $ Text $ show score 
        ,  Color white $ translate (0) 250 $ Scale 0.3 0.3 $ Text $ show timer
        ] ++ (genRectagles field rectPositions picks)

render picks _ _ p3 _ _(Game field rectPositions _ _ _ score _ CardPreview _) = pictures $ 
        [
            p3
        ,  Color white $ translate (-285) 280 $ Scale 0.6 0.6 $ Text $ show score
        ] ++ (genRectagles field rectPositions picks)

render picks _ _ p3 _ _(Game field rectPositions _ _ timer score _ ChekingCard _) = pictures $ 
        [
           p3
        ,  Color white $ translate (-285) 280 $ Scale 0.6 0.6 $ Text $ show score 
        ,  Color white $ translate (0) 250 $ Scale 0.3 0.3 $ Text $ show timer
        ] ++ (genRectagles field rectPositions picks)

render picks _ _ _ p4 _(Game field rectPositions _ _ _ score _ GamePaused _) = pictures $ 
        [
            p4
        ,   Color white $ translate (-285) 280 $ Scale 0.6 0.6 $ Text $ show score
        ]

render _ _ _ _ _ p5(Game _ _ _ _ timer score _ GameFinished _) = pictures
        [
           p5
        ,  Color white $ translate 30 80 $ Scale 0.6 0.6 $ Text $ show score 
        ,  Color white $ translate 30 (-15) $ Scale 0.6 0.6 $ Text $ show timer
        ]
-- ____________________________________________________________________________________________________________________________

<<<<<<< HEAD
=======
mWindow :: Display
mWindow = InWindow "Memory Puzzle" (width, height) (offset, offset)  
>>>>>>> 0a9eceafa3957b2176db900a1affadb0279f5a9d
    



-- Обновление MemoryPuzzleGame каждый фрейм в зависимости от состояния
-- ____________________________________________________________________________________________________________________________


update :: Float -> MemoryPuzzleGame -> MemoryPuzzleGame
update _ (Game field rectPositions firstSelectedCard secondSelectedCard timer score difficult ChekingCard 0) = Game
            { 
                field = fst $ openResult
            ,   rectPositions = rectPositions
            ,   firstSelectedCard = (-1,-1)
            ,   secondSelectedCard = (-1,-1)
<<<<<<< HEAD
            ,   timer = timer
            ,   score = if snd openResult == True then score +100 else score - 10
            ,   difficult = difficult
=======
            ,   timer = time
            ,   score = if snd openResult == True then sc + 50 else sc - 10
            ,   difficult = mode
>>>>>>> 0a9eceafa3957b2176db900a1affadb0279f5a9d
            ,   status = GameStarted
            ,   using = 0
            }
                where openResult = checkOpened (firstSelectedCard,secondSelectedCard) countOfPicks field


update _ (Game field rectPositions firstSelectedCard secondSelectedCard timer score difficult CardPreview 0)  = Game
            { 
                field = closeAll field
            ,   rectPositions = rectPositions
            ,   firstSelectedCard = firstSelectedCard
            ,   secondSelectedCard = secondSelectedCard
            ,   timer = startTime
            ,   score = score
            ,   difficult = difficult
            ,   status = GameStarted
            ,   using = 0
            }

update _ (Game field rectPositions firstSelectedCard secondSelectedCard timer score difficult GameStarted using) = Game
            {
                field = field
            ,   rectPositions = rectPositions
            ,   firstSelectedCard = firstSelectedCard
            ,   secondSelectedCard = secondSelectedCard
            ,   timer = timer - 1
            ,   score = score
            ,   difficult = difficult
            ,   status = if endGame then GameFinished else GameStarted
            ,   using = if (using > 0 ) then using-1 else 0
            }
                 where endGame = (timer == 1) ||(isAllFounded field)  

update _ Game{..}  = Game
            { 
                field = field
            ,   rectPositions = rectPositions
            ,   firstSelectedCard = firstSelectedCard
            ,   secondSelectedCard = secondSelectedCard
            ,   timer = timer
            ,   score = score
            ,   difficult = difficult
            ,   status = status
            ,   using = if (using > 0 ) then using-1 else 0
            }
-- ____________________________________________________________________________________________________________________________


 
mWindow :: Display
mWindow = InWindow "Memory Puszzle" (width, height) (offset, offset)           


main :: IO ()
main = do
    bGroundMainMenu       <- loadBMP "Backgrounds\\mainMenu.bmp"
    bGroundModeSelection  <- loadBMP "Backgrounds\\modeSelection.bmp"
    bGroundGameInProgress <- loadBMP "Backgrounds\\gameInProgress.bmp"
    bGroundGamePaused     <- loadBMP "Backgrounds\\pause.bmp"
    bGroundGameResult     <- loadBMP "Backgrounds\\gameResults.bmp"
    picks <- loadIcons countOfPicks
    gen <- newStdGen
    play mWindow background fps (initialNewGame gen) (render picks bGroundMainMenu bGroundModeSelection bGroundGameInProgress bGroundGamePaused bGroundGameResult) (handleKeys gen) update