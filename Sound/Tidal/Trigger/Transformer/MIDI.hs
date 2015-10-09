module Sound.Tidal.Trigger.Transformer.MIDI where

import Data.Maybe

import Sound.Tidal.Trigger.Types

midi2double :: Input -> Input
midi2double i = IVDouble (((/ 127) $ fromIntegral $ ((fromJust $ i_get i) :: Int)) :: Double)
