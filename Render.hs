{------------------------------------------}
{-                SpinPong                -}
{------------------------------------------}

module Render (
    renderGame
) where

import State
import Control.Monad
import Graphics.UI.SDL as SDL

sdlRectFromRectangle :: Rectangle -> Maybe SDL.Rect
sdlRectFromRectangle (Rectangle x y w h) = Just (SDL.Rect (floor x) (floor y) (floor w) (floor h))

-- Renders the given GameState to the supplied surface.
renderGame :: Int -> Int -> Surface -> GameState -> IO ()
renderGame screenWidth screenHeight surface state = do

    -- Background
    SDL.fillRect surface
        (Just (SDL.Rect 0 0 screenWidth screenHeight))
        (SDL.Pixel 0x00000000)
        
    -- Ball
    SDL.fillRect surface
        (sdlRectFromRectangle . ballRect . gameBall $ state)
        (SDL.Pixel 0x00FFFFFF)
    
    -- Paddle one
    SDL.fillRect surface
        (sdlRectFromRectangle . paddleRect . gamePaddleOne $ state)
        (SDL.Pixel 0x00FFFFFF)
    
    -- Paddle two
    SDL.fillRect surface
        (sdlRectFromRectangle . paddleRect . gamePaddleTwo $ state)
        (SDL.Pixel 0x00FFFFFF)
        
    -- Draw the score to the caption of the window because I'm lazy.
    let captionString = "SpinPong: " ++ (show . gameScoreOne $ state) ++ " : " ++ (show . gameScoreTwo $ state)
    SDL.setCaption captionString captionString
        
    return ()
