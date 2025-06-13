{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, MultiWayIf, BlockArguments #-}
module Main where

import Clash.Prelude

import FlappySquare
import RetroClash.Sim.SDL
import Control.Monad
import Control.Monad.State

main :: IO ()
main =
    flip evalStateT initState{ scrollOffset = 40, birdY = 150 } $
    withMainWindow videoParams \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        modify \st@MkSt{..} -> if
            | keyDown ScancodeRight -> st{ scrollOffset = satAdd SatWrap scrollOffset 1 }
            | keyDown ScancodeLeft -> st{ scrollOffset = satSub SatWrap scrollOffset 1 }
            | keyDown ScancodeDown -> st{ birdY = birdY + 1 }
            | keyDown ScancodeUp -> st{ birdY = birdY - 1 }
            | keyDown ScancodeSpace -> st{ gameOver = not gameOver }
            | otherwise -> st
        gets $ rasterizePattern . draw
  where
    videoParams = MkVideoParams
        { windowTitle = "Flappy Square"
        , screenScale = 2
        , screenRefreshRate = 60
        , reportFPS = True
        }
