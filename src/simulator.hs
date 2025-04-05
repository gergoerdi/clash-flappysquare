{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude

import FlappySquare
import RetroClash.Sim.SDL
import Control.Monad
import Control.Monad.State

main :: IO ()
main =
    flip evalStateT initState $
    withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        modify $ updateState $ keyDown ScancodeSpace
        gets $ rasterizePattern . draw
  where
    videoParams = MkVideoParams
        { windowTitle = "Flappy Square"
        , screenScale = 2
        , screenRefreshRate = 60
        , reportFPS = True
        }
