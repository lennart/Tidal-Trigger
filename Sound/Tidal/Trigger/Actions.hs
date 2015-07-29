module Sound.Tidal.Trigger.Actions where

import qualified Sound.Tidal.Context as T


import Data.List
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
  trig' <- pushVStack vol trig
  trig'' <- pushStack sample trig'
  tickPattern trig stream' shape (combinePattern trig'') ((length $ stack trig'') - 1)
  return trig''


-- allow adding rests
pushRest e trig = do
  pushStack "~" trig

-- pop stack and playback
playStack e trig = do
  let (shape, stream') = dest trig
  tickPattern trig stream' shape (combinePattern trig) 0
  return trig



combinePattern trig = foldl (T.|+|) (T.sound snd) [T.gain gain'', T.speed speed'']
  where
    tpc = cycleResolution trig
    pick' = pick trig
    pick'' = (T.p $ show pick') :: T.Pattern Int
    stack' = take tpc $ stack trig
    stack'' = T.p $ intercalate " " stack'
    speed' = fromEnum $ dir trig
    speed'' = T.p $ show $ speed'
    gain' = map show $ take tpc $ vstack trig
    gain'' = T.p $ intercalate " " gain'
    snd = T.pick T.<$> stack'' T.<*> pick''

setDirection e trig = do
  let trig' = trig { dir = toEnum $ val e }
--  putStrLn ("Dir: " ++ (show $ dir trig'))
  return $ trig'

-- What is time? Time is standing still until you move it!
-- step a
-- tickpattern on rotary location call
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

playSlice e trig = do
  let (shape, stream') = dest trig
      val' = abs (val e)
      playhead' = playhead trig
      trig' = trig { stack = rotate val' $ stack trig }
  tickPattern trig stream' shape (combinePattern trig') val'
  return trig

pickSample e trig = do
  let val' = (val e)
  return trig { pick = (floor $ (((fromIntegral $ val') / 127.0) * 16)) }

enterBrackets e trig = do
  let trig' = trig { fifo = (['['] ++ (fifo trig)) }
  return trig'

leaveBrackets e trig = do
  let trig' = trig { fifo = (dropWhile (/= '[') (fifo trig)) }
  return trig'

setCycleRes e trig = do
  putStrLn ("Set Cycle Resolution: " ++ (show cres))
  return $ trig { cycleResolution = cres }
    where
      cres = floor (((fromIntegral (val e)) / 127) * 20)

setTempo e trig = do
  return $ trig { tempo = tempo' }
    where
      val' = val e
      val'' = fromIntegral $ mod val' 500
      tempo' = 1 - (val'' / 500)
