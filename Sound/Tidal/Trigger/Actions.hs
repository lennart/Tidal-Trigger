module Sound.Tidal.Trigger.Actions where

import qualified Sound.Tidal.Context as T

import Sound.Tidal.Trigger.Stream
import Sound.Tidal.Trigger.Types
import qualified Data.Map.Strict as Map

asMapping fs = Map.fromList fs

-- value conversion helper
midi2norm :: (Integral a, Fractional b) => a -> b
midi2norm a = (fromIntegral a) / 127

-- push sample to stack and trigger it
triggerSample sample e trig = do
  let vol = midi2norm $ val e
      (shape, stream') = dest trig
      pattern = (T.sound (T.p sample) T.|+| T.gain (T.p $ show vol))
  tickPattern stream' shape pattern 0
  pushStack pattern trig
  return ()

-- allow adding rests
pushRest e trig = do
  pushStack (T.sound $ T.p "~") trig
  return ()

-- pop stack and playback
playStack e trig = do
  let (shape, stream') = dest trig
  stack' <- popStack trig
  let pattern = T.cat stack'
  tickPattern stream' shape pattern 0
  return ()

-- What is time? Time is standing still until you move it!
-- step a
-- tickpattern on rotary location call
playSlice e trig = do
  let (shape, stream') = dest trig
      val' = (val e)
  stack' <- readStack trig
  tickPatternAt trig stream' shape (T.cat stack') $ val'
