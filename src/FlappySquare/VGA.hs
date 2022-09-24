{-# LANGUAGE ScopedTypeVariables, NumericUnderscores #-}
{-# LANGUAGE DuplicateRecordFields, RecordWildCards, ApplicativeDo #-}
module FlappySquare.VGA where

import Clash.Prelude
import RetroClash.Clock
import Data.Maybe (isJust)
import Data.Word (Word8)

data VGASync dom = VGASync
    { vgaHSync :: "HSYNC" ::: Signal dom Bit
    , vgaVSync :: "VSYNC" ::: Signal dom Bit
    , vgaDE    :: "DE"    ::: Signal dom Bool
    }

data VGAOut dom = VGAOut
    { vgaSync  :: VGASync dom
    , vgaR     :: "RED"   ::: Signal dom Word8
    , vgaG     :: "GREEN" ::: Signal dom Word8
    , vgaB     :: "BLUE"  ::: Signal dom Word8
    }

data VGADriver dom w h = VGADriver
    { vgaSync :: VGASync dom
    , vgaX    :: Signal dom (Maybe (Index w))
    , vgaY    :: Signal dom (Maybe (Index h))
    }

strengthen :: forall n k. (KnownNat n, KnownNat k) => Index (n + k) -> Maybe (Index n)
strengthen x
  | x <= fromIntegral (maxBound @(Index n)) = Just $ fromIntegral x
  | otherwise = Nothing

betweenCO :: (Ord a) => (a, a) -> a -> Bool
betweenCO (lo, hi) x = lo <= x && x < hi

sync :: Bit -> Bool -> Bit
sync polarity = boolToBit . (== polarity) . boolToBit

vgaDriver
    :: (HiddenClockResetEnable dom)
    => (DomainPeriod dom ~ HzToPeriod 25_175_000)
    => VGADriver dom 640 480
vgaDriver = VGADriver{ vgaSync = VGASync{..}, .. }
  where
    stateH = register (0 :: Index 800) $ satAdd SatWrap 1 <$> stateH
    endLine = stateH .==. pure maxBound
    stateV = regEn (0 :: Index 524) endLine $ satAdd SatWrap 1 <$> stateV

    -- vgaX = mux (stateH .<. 640) (Just . fromIntegral <$> stateH) (pure Nothing)
    -- vgaY = mux (stateV .<. 480) (Just . fromIntegral <$> stateV) (pure Nothing)
    vgaX = strengthen <$> stateH
    vgaY = strengthen <$> stateV

    vgaHSync = sync low . betweenCO (656, 752) <$> stateH
    vgaVSync = sync low . betweenCO (491, 493) <$> stateV
    vgaDE = isJust <$> vgaX .&&. isJust <$> vgaY

type Color = (Word8, Word8, Word8)

vgaOut
    :: (HiddenClockResetEnable dom)
    => VGASync dom
    -> Signal dom Color
    -> VGAOut dom
vgaOut vgaSync@VGASync{..} rgb = VGAOut{..}
  where
    (vgaR, vgaG, vgaB) = unbundle $ blank rgb

    blank = mux (not <$> vgaDE) (pure (0, 0, 0))
