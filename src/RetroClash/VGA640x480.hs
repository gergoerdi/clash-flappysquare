{-# LANGUAGE ScopedTypeVariables, NumericUnderscores #-}
{-# LANGUAGE DuplicateRecordFields, RecordWildCards, ApplicativeDo #-}
module RetroClash.VGA640x480 where

import Clash.Prelude
import RetroClash.Clock
import Data.Maybe (isJust)
import Data.Word (Word8)
import Clash.Class.Counter

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

between :: (Ord a) => a -> (a, a) -> Bool
x `between` (lo, hi) = lo <= x && x <= hi

sync :: Bit -> Bool -> Bit
sync polarity b = if b then polarity else complement polarity

type ScreenWidth = 640
type ScreenHeight = 480

vgaDriver640x480at60
    :: (HiddenClockResetEnable dom)
    => (DomainPeriod dom ~ HzToPeriod 25_175_000)
    => VGADriver dom ScreenWidth ScreenHeight
vgaDriver640x480at60 = VGADriver{ vgaSync = VGASync{..}, .. }
  where
    cnt = register (0, 0) $ countSucc @(Index 524, Index 800) <$> cnt
    (vcount, hcount) = unbundle cnt

    vgaX = strengthen <$> hcount
    vgaY = strengthen <$> vcount

    vgaHSync = sync low . (`between` (640 + 16, 640 + 16 + 96 - 1)) <$> hcount
    vgaVSync = sync low . (`between` (480 + 11, 480 + 11 + 2 - 1)) <$> vcount
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
