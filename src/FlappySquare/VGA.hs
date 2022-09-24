{-# LANGUAGE ScopedTypeVariables, NumericUnderscores #-}
{-# LANGUAGE DuplicateRecordFields, RecordWildCards, ApplicativeDo #-}
module FlappySquare.VGA where

import Clash.Prelude
import Clash.Class.HasDomain
import Clash.Class.Counter
import RetroClash.Clock
-- import RetroClash.Utils
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

nextIdx :: (Eq a, Enum a, Bounded a) => a -> a
nextIdx x
  | x == maxBound = minBound
  | otherwise = succ x

betweenCO :: (Ord a) => (a, a) -> a -> Bool
betweenCO (lo, hi) x = lo <= x && x < hi

sync :: Bool -> Bit
sync = complement . boolToBit

vgaDriver
    :: (HiddenClockResetEnable dom)
    => (DomainPeriod dom ~ HzToPeriod 25_175_000)
    => VGADriver dom 640 480
vgaDriver = VGADriver{ vgaSync = VGASync{..}, .. }
  where
    cnt = register (0, 0) $ countSucc @(Index 524, Index 800) <$> cnt
    (cntV, cntH) = unbundle cnt

    vgaX = strengthen <$> cntH
    vgaY = strengthen <$> cntV

    vgaHSync = sync . betweenCO (656, 752) <$> cntH
    vgaVSync = sync . betweenCO (491, 493) <$> cntV
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
