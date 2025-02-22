{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Clash.Prelude

import FlappySquare.Circuit
import RetroClash.Utils
import RetroClash.VGA (vga640x480at60)
import RetroClash.VGA640x480
import RetroClash.Sim.IO
import RetroClash.Sim.SDL
import RetroClash.Sim.VGA
import RetroClash.Sim.VGASDL
import Control.Monad
import Control.Monad.State
import Control.Monad.Loops

board' btn =
    let VGAOut{ vgaSync = VGASync{..}, ..} = topEntity clockGen resetGen btn
    in bundle (vgaHSync, vgaVSync, bitCoerce <$> bundle (vgaR, vgaG, vgaB))

main :: IO ()
main = do
    buf <- newBufferArray

    sim <- simulateIO board' (toActive False)

    flip evalStateT initSink $ withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        let btn = toActive $ keyDown ScancodeSpace

        untilM_ (return ()) $ do
            sim $ \vgaOut -> do
                frameDone <- vgaSinkBuf vga640x480at60 buf vgaOut
                return (btn, frameDone)

        return $ rasterizeBuffer buf
  where
    videoParams = MkVideoParams
        { windowTitle = "Pong (Clash)"
        , screenScale = 2
        , screenRefreshRate = 60
        , reportFPS = True
        }
