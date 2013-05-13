{------------------------------------------}
{-                SpinPong                -}
{------------------------------------------}

module Behaviors (
    stepGame
) where

import State
import Input


-- Some control constants for the behavior of the paddles.
paddleAccel = 0.8
paddleDecel = 0.9
spinNumber  = 500


-- Given two Rectangles, this function will return true if they are intersecting.
rectanglesIntersect :: Rectangle -> Rectangle -> Bool
rectanglesIntersect (Rectangle x y w h) (Rectangle x' y' w' h') =
    not $ (x  + w ) < x'
       || (x' + w') < x
       || (y  + h ) < y'
       || (y' + h') < y

       
-- If the given rectangle is outside the bounds of the screen defined in State.hs, a restored rectangle is returned.
-- If the rectangle is safely inside the screen's bounds then Nothing is returned.
keepRectOnScreen :: Rectangle -> Maybe Rectangle 
keepRectOnScreen (Rectangle x y w h) = if offTop || offBottom || offLeft || offRight
                                     then Just restoredRect else Nothing                
    where
        offTop = y < 0
        offBottom = y + h > fromIntegral screenHeight
        offLeft = x < 0
        offRight = x + w > fromIntegral screenWidth
        
        restoredRect = (Rectangle (if offLeft then 0 else if offRight then fromIntegral screenWidth - w else x)
                                  (if offTop  then 0 else if offBottom then fromIntegral screenHeight - h else y) w h)
                                  

-- Moves a paddle forward one step, adjusting speed and position, given some input from the player.
movePaddle :: PlayerInputState -> Paddle -> Paddle
movePaddle input (Paddle speed  (Rectangle x y  w h)) =
                 (Paddle speed' (Rectangle x y' w h))
    where
        speed' = if btnUp   input then speed - paddleAccel
            else if btnDown input then speed + paddleAccel
            else speed * paddleDecel
        y' = y + speed'
        
        
-- If the paddle is off the edge of the screen, it gets reset and the speed set to zero.
keepPaddleOnScreen :: Paddle -> Paddle
keepPaddleOnScreen (Paddle speed rect) = 
    case keepRectOnScreen rect of
        Nothing    -> (Paddle speed rect )
        Just rect' -> (Paddle     0 rect')

        
-- Moves the given ball forwards one step taking in to account its speed and spin.
moveBall :: BallState -> BallState
moveBall (BallState angle  speed spin (Rectangle x  y  w h)) = 
         (BallState angle' speed spin (Rectangle x' y' w h))
    where
        x' = x + speed * cos angle
        y' = y + speed * sin angle
        angle' = angle + spin
        
        
-- Process collision between a paddle and a ball, and return the new ball state post-collision.
hitBallAgainstPaddle :: Paddle -> BallState -> BallState
hitBallAgainstPaddle paddle (BallState angle speed spin rect) = 
    
    if rectanglesIntersect padRect rect then
        (BallState angle' speed' spin' (Rectangle x' (rectY rect) (rectW rect) (rectH rect)))
    else
        (BallState angle speed spin rect)
    
    where
    
        padRect = paddleRect paddle
        padX  = rectX padRect
        padW  = rectW padRect
        ballX = rectX rect
        ballW = rectW rect
        
        angle' = atan2 (speed * sin angle) (-speed * cos angle)
        speed' = speed
        spin'  = spin + (if ballX < padX then 1 else (-1)) * (paddleSpeed paddle) / spinNumber
        x'     = if ballX < padX
               then padX - ballW
               else padX + padW
                            
                            
-- Process collision between a ball and the walls of the playfield.
keepBallOnScreen :: BallState -> BallState
keepBallOnScreen (BallState angle speed spin rect) =
    case keepRectOnScreen rect of
        Nothing    -> (BallState angle  speed spin rect )
        Just rect' -> (BallState angle' speed spin rect')
    where
        angle' = atan2 (-speed * sin angle) (speed * cos angle)
        
        
-- Steps the game forwards one frame given the inputs from the external world.
stepGame :: GameState -> InputState -> GameState
stepGame (GameState paddle1  paddle2  score1  score2  ball ) input = 
         (GameState paddle1' paddle2' score1' score2' ball')
    where
        paddle1' = keepPaddleOnScreen . movePaddle (inputP1 input) $ paddle1
        paddle2' = keepPaddleOnScreen . movePaddle (inputP2 input) $ paddle2
        
        steppedBall = hitBallAgainstPaddle paddle1'
                    . hitBallAgainstPaddle paddle2'
                    . moveBall
                    $ ball
                    
        sbr = ballRect steppedBall -- sbr: Stepped ball rect
                    
        leftScore  = (rectX sbr) < 0
        rightScore = (rectX sbr) + (rectW sbr) > fromIntegral screenWidth
        
        score1' = if rightScore then score1 + 1 else score1
        score2' = if  leftScore then score2 + 1 else score2

        ball' = if leftScore || rightScore then newBallStateWithAngle (ballAngle steppedBall + pi) else keepBallOnScreen steppedBall
        