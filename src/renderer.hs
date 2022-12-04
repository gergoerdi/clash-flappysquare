{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude

import FlappySquare
import Codec.Picture

import System.IO (hSetBuffering, stdout, BufferMode( NoBuffering ))
import Text.Printf
import Control.Monad.State.Strict
import Control.Monad.IO.Class

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    flip evalStateT initState $ do
        forM_ (zip [(1 :: Int)..] $ toFrames buttonPresses) $ \(i, btn) -> do
            modify $ updateState btn
            render <- gets draw
            let render' x y = let (r, g, b) = render (fromIntegral x) (fromIntegral y)
                              in PixelRGB8 r g b
            liftIO $ writePng (printf "flappysquare-render-%03d.png" i) $ generateImage render' 640 480
            when (i `mod` 10 == 0) $ liftIO $ putStr "."
        liftIO $ putStrLn ""

buttonPresses :: [Int]
buttonPresses = [0, 0, 20, 0, 0, 35, 0, 0, 0, 0, 0, 0, 0, 0, 30, 0, 0, 20]

toFrames :: [Int] -> [Bool]
toFrames = (<> replicate 30 False) . concatMap (\i -> replicate i False <> [True])
