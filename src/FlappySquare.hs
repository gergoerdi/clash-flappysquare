{-# LANGUAGE RecordWildCards #-}
module FlappySquare where

import Clash.Prelude
import RetroClash.VGA640x480 (Color, ScreenWidth, ScreenHeight, between)

type PipeGap = 4
type PipeWidth = 64
type NumPipes = 4


data St = MkSt
    { birdY      :: !(Signed 10)
    , birdSpeed  :: !(Signed 10)
    , pipeOffset :: !(Index (NumPipes * PipeWidth * PipeGap))
    , gameOver   :: !Bool
    }
    deriving (Show, Generic, NFDataX)

initState :: St
initState = MkSt
    { pipeOffset = 0
    , birdSpeed  = 0
    , birdY      = 200
    , gameOver   = False
    }

birdX :: Index ScreenWidth
birdX = 100

around :: (Ord a, Num a) => a -> (a, a) -> Bool
x `around` (p, r) = x `between` (p - r, p + r)

birdWidth :: Index ScreenWidth
birdWidth = 20

birdHeight :: Signed 10
birdHeight = 20

pipes :: Vec NumPipes (Index ScreenHeight, Index ScreenHeight)
pipes =
    (130, 290) :>
    (80,  280) :>
    (60,  270) :>
    (90,  320) :>
    Nil

updateState :: Bool -> St -> St
updateState btn st@MkSt{..}
  | gameOver = initState
  | otherwise = st
    { pipeOffset = satAdd SatWrap 1 pipeOffset
    , birdSpeed = if btn then birdSpeed - 5 else birdSpeed + 1
    , birdY = birdY + birdSpeed `shiftR` 3
    , gameOver = not birdClear
    }
  where
    (top1, bottom1, _) = pipeAt (birdX - birdWidth) st
    (top2, bottom2, _) = pipeAt (birdX + birdWidth) st
    birdClear = all (birdY `between`)
        [ (fromIntegral top1 + birdHeight, fromIntegral bottom1 - birdHeight)
        , (fromIntegral top2 + birdHeight, fromIntegral bottom2 - birdHeight)
        ]

pipeAt :: Index ScreenWidth -> St -> (Index ScreenHeight, Index ScreenHeight, Index 64)
pipeAt x MkSt{..} = (top, bottom, offset)
  where
    idx :: Index NumPipes
    gap :: Index PipeGap
    (idx, gap, offset) = bitCoerce (satAdd SatWrap (fromIntegral x) pipeOffset)

    (top, bottom)
        | gap == maxBound = pipes !! idx
        | otherwise = (minBound, maxBound)

draw :: St -> Index ScreenWidth -> Index ScreenHeight -> Color
draw st@MkSt{..} x y
    | isBird = yellow
    | isPipe = pipeColor
    | otherwise = if gameOver then red else blue
  where
    isBird =
        x `around` (birdX, birdWidth) &&
        fromIntegral y `around` (birdY, birdHeight)

    isPipe = not (fromIntegral y `between` (top, bottom))
    (top, bottom, offset) = pipeAt x st
    pipeColor
        | offset < (minBound + 2)  = gray
        | offset < 10              = lightGreen
        | offset > (maxBound - 2)  = gray
        | offset > (maxBound - 10) = darkGreen
        | otherwise                = green

blue, yellow, red, gray, green, lightGreen, darkGreen :: Color
blue = (0x40, 0x80, 0xf0)
yellow = (0xf0, 0xe0, 0x40)
red = (0x80, 0x00, 0x00)
green = (0x92, 0xe2, 0x44)
lightGreen = (0xa5, 0xff, 0x4d)
darkGreen = (0x73,0xb3, 0x36)
gray = (0x30, 0x30, 0x30)
