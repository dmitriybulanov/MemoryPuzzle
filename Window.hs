import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Kernel
import System.Random
import Data.Matrix


width, height, offset :: Int
width = 1024
height = 768
offset = 0

background :: Color
background = white

fps :: Int
fps = 60




data MemoryPuzzleGame = Game
    {
      field :: Int
    , score :: Int
    , started :: Bool
    } deriving Show

    
initialNewGame :: Int -> Int ->  MemoryPuzzleGame
initialNewGame n m = Game   
    {
        field = n * m
    ,   score = 0
    ,   started = False
    }   

    
    
    
drawingMainWindow :: Picture
drawingMainWindow  = pictures
  [ translate (-285) 220 $ Scale 0.6 0.6 $ Text "Memory Puzzle"
  , Line [(-300,180), (300, 180)]
  , translate (-120) 20 $ (Text "Start")
  , translate (-95) (-190) $ Text "End"
  ]


checkOnClickOnStart :: (Float,Float) -> Bool 
checkOnClickOnStart (x,y) = if( x > -120 && x < 120) then if ( y > 0 && y < 150) then True else False  else False
  
    
handleKeys :: Event -> MemoryPuzzleGame -> MemoryPuzzleGame
handleKeys (EventKey (MouseButton LeftButton) _ _ position) (Game f sc False)
        | (checkOnClickOnStart position) == True = Game
            {
                field = f
            ,   score = sc
            ,   started = True
            }
        | otherwise = (Game f sc False)
handleKeys _ mGame = mGame

    

render :: MemoryPuzzleGame -> Picture
render  (Game _ _ False)  = drawingMainWindow 
render  (Game _ sc True)  = pictures
  [ translate (-285) 220 $ Scale 0.5 0.5 $ Text ("Scores : " ++ show sc)
  , Line [(-300,180), (300, 180)]
  ]

    
mWindow :: Display
mWindow = InWindow "Memory Puszzle" (width, height) (offset, offset)  
    
update :: Float -> MemoryPuzzleGame -> MemoryPuzzleGame
update _ mGame = mGame
 

main :: IO ()
main = do
    play mWindow background fps (initialNewGame 300 300) render handleKeys update
    