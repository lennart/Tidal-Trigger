module Sound.Tidal.Trigger.Stream (
  tickPattern,
  tickPatternAt,
  pushStack,
  popStack,
  peekFifo,
  pushVStack
  ) where

import qualified Sound.Tidal.Context as T
import qualified Sound.PortMidi as PM

import Control.Monad
import Control.Concurrent
import Control.Exception as E
import Foreign.C
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Map.Strict as Map

import Sound.OSC.FD
import Sound.Tidal.Stream
import Sound.Tidal.Utils

import GHC.Float (float2Double, double2Float)

import Sound.Tidal.Trigger.Responder
import Sound.Tidal.Trigger.Types

-- generics

tickPattern trig stream shape pattern vel = do
  let pattern' = pattern -- |+| ((tvel shape) vel)
  now <- getCurrentTime
  let tempo' = T.Tempo now 0 (tempo trig)
  tOnTick stream shape pattern tempo' 0 1

tickPatternAt trig stream shape pattern tick = do
  now <- getCurrentTime
  let tempo' = T.Tempo now (fromIntegral tick) 0.125 -- (tempo trig)
  tOnTickAt trig stream shape pattern tempo' tick

tOnTick s shape pattern change ticks len
  = do
       let ticks' = (fromIntegral ticks) :: Integer
           a = ticks' T.% T.ticksPerCycle
           b = (ticks' + 1) T.% T.ticksPerCycle
           messages = mapMaybe
                      (T.toMessage s shape change ticks)
                      (T.seqToRelOnsets (0, len) pattern)
       E.catch (sequence_ messages) (\msg -> putStrLn $ "oops " ++ show (msg :: E.SomeException))
       return ()


toMessageAt s shape change tpc tick now (o, m) =
  do m' <- applyShape' shape m
     let oscdata = cpsPrefix ++ preamble shape ++ (parameterise $ catMaybes $ mapMaybe (\x -> Map.lookup x m') (params shape))
   --  putStrLn ("time: " ++ (show logicalNow) ++ "\tperiod: " ++ (show logicalPeriod) ++ "\tons: " ++ (show logicalOnset))
     return $ mkmsg oscdata
       where
         parameterise :: [Datum] -> [Datum]
         parameterise ds | namedParams shape =
           mergelists (map (string . name) (params shape)) ds
                         | otherwise = ds
         cpsPrefix | cpsStamp shape = [float (T.cps change)]
                   | otherwise = []
         nudge = maybe 0 (toF) (Map.lookup (F "nudge" (Just 0)) m)
         toF (Just (Float f)) = float2Double f
         toF _ = 0
         cycleD = ((fromIntegral tick) / (fromIntegral tpc)) :: Double
         logicalNow = (T.logicalTime change cycleD)
         logicalPeriod = (T.logicalTime change (cycleD + (1/(fromIntegral tpc)))) - logicalNow
         logicalOnset = logicalNow + (logicalPeriod * o) + (latency shape) + nudge
--     let logicalNow = realToFrac $ utcTimeToPOSIXSeconds now
  --       logicalPeriod =
         sec = floor logicalOnset
         usec = floor $ 1000000 * (logicalOnset - (fromIntegral sec))
         mkdata x = ((int32 sec):(int32 usec):x)

         mkmsg x | timestamp shape == BundleStamp = sendOSC s $ Bundle (ut_to_ntpr logicalOnset) [Message (path shape) x]
             | timestamp shape == MessageStamp = sendOSC s $ Message (path shape) (mkdata x)
             | otherwise = doAt logicalOnset $ sendOSC s $ Message (path shape) x

tOnTickAt trig s shape pattern change ticks
  = do
       now <- getCurrentTime
       let ticks' = fromIntegral ticks :: Integer
           tpc = cycleResolution trig
           a = ticks' T.% (fromIntegral tpc)
           b = (ticks' + 1) T.% (fromIntegral tpc)
           ons = T.seqToRelOnsets (a, b) pattern
           -- only select the first message, so we only emit one sample per tick of the rotary
           ons' = case length ons of
             0 -> []
             _ -> [head ons]
           messages = mapMaybe
                      (toMessageAt s shape change tpc ticks' now)
                      ons
       putStrLn ("OnTick: " ++ (show ticks'))
       E.catch (sequence_ messages) (\msg -> putStrLn $ "oops " ++ show (msg :: E.SomeException))
       return ()



-- stack actions


-- push to pattern stack
pushStack sample trig = do
  let stack' = stack trig
  return $ trig { stack = ([sample] ++ stack') }

pushVStack v trig = do
  let vstack' = vstack trig

  return $ trig { vstack = [v] ++ vstack' }

-- return trigger with emptied stack
popStack trig = do
  return $ trig { stack = [] }

peekFifo trig = do
  let f = fifo trig
  case length f of
       0 -> Nothing
       _ -> Just $ head f

-- step b
-- allow multiple input types triggers (hook up arduino or other midi controller to control "time" via rotary, to avoid skipping)

{-
 step b.1

  serialIn "/dev/tty.usbserial"
  midiIn "QUNEO"
  oscIn "127.0.0.1" 7771

-}


-- 2. step
-- allow holding play button to repeat pattern / no
-- allow adjusting speed by slider / no
-- allow entering braces mode (e.g. for [], {}, or () in case of poly rythms) while holding another (non drumpad) button
{-

- make each action able to change the trigger (i.e. the state) / check

- add a brace mode stack

- change the behavior of other triggers according to current brace top of brace stack (i.e. make new samples be pushed into a "substack" that will be merged into a [] or {} group when the brace mode is left
- add actions on note on to enter and note off to leave brace modes

-}
-- add a "comma" button to add multiple patterns into a brace
-- special note: when adding polyrythms for bjorklund, drum pads jump to numbers and accept two successive values (starting at 1 to 17), e.g. 5 8
-- add quantifier (e.g. for adding bd*8) think of how, maybe not needed

-- 3. step
-- allow adding modifiers while (maybe only while play is pressed)
-- -- drum pads are now mapped to each modifier (speed, coarse, or for midi: cutoff, resonance, lfo1rate)
-- -- pressing a pad will add the modifier to the current pattern, using x/y latch values to set the modifier
-- -- pressing the padd again will add another value to the modifier stack
-- -- pressing another pad will add another modifier to the current pattern...

-- 4.step
-- allow instant modification of patterns for modifiers and sampels pattern
-- allow samples "bd sn" (run 16)  style of looping to pattern samples
-- -- use long slider width to adjust the run value (this should be a live control

-- 5. step
-- think of how to make melody making possible like: (50+).(2*).negate <$> (run x)
-- use a stack for monadic operations on a simple number generator
--
