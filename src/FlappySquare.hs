{-# LANGUAGE RecordWildCards #-}
module FlappySquare where

import Clash.Prelude
import RetroClash.VGA640x480 (Color, ScreenWidth, ScreenHeight, between)

data St = MkSt
    { wallOffset :: Index 640
    , birdSpeed :: Signed 10
    , birdY :: Signed 10
    , gameOver :: Bool
    }
    deriving (Show, Generic, NFDataX)

initState :: St
initState = MkSt
    { wallOffset = 0
    , birdSpeed = 0
    , birdY = 200
    , gameOver = False
    }

birdX :: Index ScreenWidth
birdX = 100

walls :: Vec 10 (Index ScreenHeight, Index ScreenHeight)
walls =
    (130, 290) :>
    (80,  280) :>
    (60,  270) :>
    (90,  320) :>
    (110, 430) :>
    (200, 410) :>
    (230, 400) :>
    (130, 380) :>
    (90,  320) :>
    (110, 280) :>
    Nil

updateState :: Bool -> St -> St
updateState btn s@MkSt{..}
  | gameOver = initState
  | otherwise = s
    { wallOffset = satAdd SatWrap 1 wallOffset
    , birdSpeed = birdSpeed + if btn then -15 else 3
    , birdY = birdY + birdSpeed `shiftR` 3
    , gameOver = not $ birdY `between` (fromIntegral top + 20, fromIntegral bottom - 20)
    }
  where
    (top, bottom, offset) = wallAt birdX s

wallAt :: Index ScreenWidth -> St -> (Index ScreenHeight, Index ScreenHeight, Index 64)
wallAt x MkSt{..} = (top, bottom, offset)
  where
    idx :: Index 10
    (idx, offset) = bitCoerce $ satAdd SatWrap x wallOffset
    (top, bottom) = walls !! idx

draw :: St -> Index ScreenWidth -> Index ScreenHeight -> Color
draw s@MkSt{..} x y
    | isBird = yellow
    | isWall = if offset < 2 then gray else if offset < 10 then lightGreen else green
    | otherwise = if gameOver then red else blue
  where
    isBird =
        x `between` (80, 120) &&
        fromIntegral y `between` (birdY - 20, birdY + 20)

    isWall = not $ y `between` (top, bottom)
    (top, bottom, offset) = wallAt x s

blue, yellow, red, gray, green, lightGreen :: Color
blue = (0x40, 0x80, 0xf0)
yellow = (0xf0, 0xe0, 0x40)
red = (0x80, 0x00, 0x00)
green = (0x92, 0xe2, 0x44)
lightGreen = (0xa5, 0xff, 0x4d)
gray = (0x30, 0x30, 0x30)
