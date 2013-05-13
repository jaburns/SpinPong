{------------------------------------------}
{-                SpinPong                -}
{------------------------------------------}

module Input (
    PlayerInputState(..),
    InputState(..),
    readInputEvent,
    newInputState
) where

import Graphics.UI.SDL as SDL

-- Defines the current state of a single player's input.
data PlayerInputState = PlayerInputState {
    btnUp   :: Bool,
    btnDown :: Bool
}

-- Defines the current state of all valid input to the game.
data InputState = InputState {
    inputP1 :: PlayerInputState,
    inputP2 :: PlayerInputState
}

-- Game key mappings.
key_inputP1up   = SDLK_UP
key_inputP1down = SDLK_DOWN
key_inputP2up   = SDLK_LEFT
key_inputP2down = SDLK_RIGHT

-- Creates an InputState data structure in a default configuration.
newInputState :: InputState
newInputState = InputState (PlayerInputState False False) (PlayerInputState False False)

-- Applies an SDL input event to an InputState structure to compute the new state of inputs.
readInputEvent :: InputState -> Event -> InputState
readInputEvent (InputState (PlayerInputState u1 d1) (PlayerInputState u2 d2)) (KeyDown k)
    | symKey k == key_inputP1up   = InputState (PlayerInputState True d1) (PlayerInputState u2 d2)
    | symKey k == key_inputP1down = InputState (PlayerInputState u1 True) (PlayerInputState u2 d2)
    | symKey k == key_inputP2up   = InputState (PlayerInputState u1 d1) (PlayerInputState True d2)
    | symKey k == key_inputP2down = InputState (PlayerInputState u1 d1) (PlayerInputState u2 True)
    | otherwise = (InputState (PlayerInputState u1 d1) (PlayerInputState u2 d2))
readInputEvent (InputState (PlayerInputState u1 d1) (PlayerInputState u2 d2)) (KeyUp k)
    | symKey k == key_inputP1up   = InputState (PlayerInputState False d1) (PlayerInputState u2 d2)
    | symKey k == key_inputP1down = InputState (PlayerInputState u1 False) (PlayerInputState u2 d2)
    | symKey k == key_inputP2up   = InputState (PlayerInputState u1 d1) (PlayerInputState False d2)
    | symKey k == key_inputP2down = InputState (PlayerInputState u1 d1) (PlayerInputState u2 False)
    | otherwise = (InputState (PlayerInputState u1 d1) (PlayerInputState u2 d2))
readInputEvent oldState _ = oldState

