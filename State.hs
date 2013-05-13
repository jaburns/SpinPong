{------------------------------------------}
{-                SpinPong                -}
{------------------------------------------}

module State (
    screenWidth,
    screenHeight,
    Angle,
    Speed,
    Paddle(..), 
    Rectangle(..),
    BallState(..),
    GameState(..),
    newBallState,
    newBallStateWithAngle,
    newGameState
) where

screenWidth  = 640 :: Int
screenHeight = 480 :: Int

type Distance = Float
type Angle    = Float
type Speed    = Float
type Spin     = Float
type Score    = Int

data Rectangle = Rectangle {
    rectX :: Distance,
    rectY :: Distance,
    rectW :: Distance,
    rectH :: Distance
}

data Paddle = Paddle {
    paddleSpeed :: Speed,
    paddleRect  :: Rectangle
}

data BallState = BallState {
    ballAngle :: Angle,
    ballSpeed :: Speed,
    ballSpin  :: Spin,
    ballRect  :: Rectangle
}

data GameState = GameState {
    gamePaddleOne :: Paddle,
    gamePaddleTwo :: Paddle,
    gameScoreOne :: Score,
    gameScoreTwo :: Score,
    gameBall :: BallState
}


-- Paddle and ball configuration.
paddleWidth   = 10
paddleHeight  = 70
ballSize      = 10
ballInitSpeed = 10

sw = fromIntegral screenWidth
sh = fromIntegral screenHeight

-- Function for creating a fresh BallState.
newBallState :: BallState
newBallState = newBallStateWithAngle 0

-- Function for creating a new BallState after scoring.
newBallStateWithAngle :: Angle -> BallState
newBallStateWithAngle theta = (BallState theta ballInitSpeed 0 (Rectangle (sw/2) (sh/2) ballSize ballSize))

-- Function for generating a fresh gamestate.
newGameState :: GameState
newGameState = (GameState
                    (Paddle 0 (Rectangle                 0  paddleInitY paddleWidth paddleHeight))
                    (Paddle 0 (Rectangle (sw - paddleWidth) paddleInitY paddleWidth paddleHeight))
                    0 0 newBallState)
    where
        paddleInitY = sh/2 - paddleHeight/2
                    