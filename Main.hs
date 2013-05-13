{------------------------------------------}
{-                SpinPong                -}
{------------------------------------------}

import Control.Monad
import Graphics.UI.SDL as SDL

import State     as S
import Input     as I
import Behaviors as B
import Render    as R

main = do 

    -- Initialize SDL and get the main surface.
    SDL.init [SDL.InitVideo]
    SDL.setCaption "SpinPong" "SpinPong"
    surface <- SDL.setVideoMode S.screenWidth S.screenHeight 24 [SDL.DoubleBuf]
    
    -- Start the game loop.
    gameLoop surface S.newGameState I.newInputState
    
    return ()
    
    where
    
        -- Consumes the SDL event queue and return the new input state and whether or not we should quit.
        processEvents :: InputState -> IO (InputState,Bool)
        processEvents inputState = do
            event <- SDL.pollEvent
            case event of
                Quit -> return (inputState,True)
                NoEvent -> return (inputState,False)
                _ -> processEvents . I.readInputEvent inputState $ event
        
        -- Main game loop.
        gameLoop :: Surface -> GameState -> InputState -> IO ()
        gameLoop surface oldGameState oldInputState = do
                
            -- Process inputs and continue only if we didn't get an SDL quit event.
            (inputState, shouldQuit) <- processEvents oldInputState
            unless shouldQuit $ do
            
                -- Step the game forwards one frame, render, and flip.
                let gameState = B.stepGame oldGameState oldInputState
                R.renderGame screenWidth screenHeight surface gameState
                SDL.flip surface
                
                -- Delay a bit to create rougly 30FPS and loop again.
                SDL.delay 33
                gameLoop surface gameState inputState
            
