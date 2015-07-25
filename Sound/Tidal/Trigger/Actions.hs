module Sound.Tidal.Trigger.Actions where

import qualified Sound.Tidal.Context as T

import Sound.Tidal.Trigger.Stream
import Sound.Tidal.Trigger.Types
import qualified Data.Map.Strict as Map

-- Mapping helper
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
  return trig

-- allow adding rests
pushRest e trig = do
  pushStack (T.sound $ T.p "~") trig
  return trig

-- pop stack and playback
playStack e trig = do
  let (shape, stream') = dest trig
  stack' <- popStack trig
  let pattern = T.sound $ T.cat stack'
  tickPattern stream' shape pattern 0
  return trig

-- What is time? Time is standing still until you move it!
-- step a
-- tickpattern on rotary location call
playSlice e trig = do
  let (shape, stream') = dest trig
      val' = (val e)
      pick' = pick trig
  stack' <- readStack trig
  let stack'' = T.cat stack'
      pick'' = (T.p $ show pick') :: T.Pattern Int
      snd = T.pick T.<$> stack'' T.<*> pick''
  tickPatternAt trig stream' shape (T.sound snd) val'
  return trig

pickSample e trig = do
  let val' = (val e)
  -- return trig { pick = (floor $ (((fromIntegral $ val') / 127.0) * 16)) }
  return trig { pick = mod val' 16 }

enterBrackets e trig = do
  let trig' = trig { fifo = (['['] ++ (fifo trig)) }
  return trig'

leaveBrackets e trig = do
  let trig' = trig { fifo = (dropWhile (/= '[') (fifo trig)) }
  return trig'
