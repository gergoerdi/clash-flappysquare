{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module FlappySquare.Video where

import FlappySquare.Game
import FlappySquare.VGA

import Clash.Prelude hiding (lift)
import RetroClash.Utils
import Data.Word

import Debug.Trace

draw :: St -> Index ScreenWidth -> Index ScreenHeight -> Color
draw MkSt{..} x y
    | isBird = yellow
    | isWall = if offset < 2 then gray else if offset < 10 then lightGreen else green
    | otherwise = if gameOver then red else blue
  where
    isBird =
        x `between` (80, 120) &&
        fromIntegral y `between` (birdY - 20, birdY + 20)

    isWall =
      y < wallsTop !! idx ||
      y > wallsBottom !! idx

    (idx, offset) = bitCoerce @_ @(Index 10, Index 64) $ satAdd SatWrap x wallOffset

white, blue, yellow, red, gray, green, lightGreen :: Color
white = (0xff, 0xff, 0xff)
blue = (0x40, 0x80, 0xf0)
yellow = (0xf0, 0xe0, 0x40)
red = (0x80, 0x00, 0x00)
green = (0x92, 0xe2, 0x44)
lightGreen = (0xa5, 0xff, 0x4d)
gray = (0x30, 0x30, 0x30)
