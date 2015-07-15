module Sound.Tidal.Trigger (
  sampleproxy,
  triggerSample,
  midiIn,
  serialIn,
  TriggerForm(..),
  Action(..),
  Trigger(stack)
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
import qualified Data.Map.Strict as Map

import Sound.OSC.FD
import Sound.Tidal.Stream
import Sound.Tidal.Utils

import GHC.Float (float2Double, double2Float)


import System.IO
import System.IO.Error
import System.Hardware.Serialport

import Sound.Tidal.Trigger.Device
import Sound.Tidal.Trigger.Responder


data TriggerEvent = TriggerOn { key :: Int, val :: Int } |
                    TriggerOff { key :: Int, val :: Int } |
                    CCChange { key :: Int, val :: Int} |
                    Serial { key :: Int, val :: Int }
                  deriving (Show)

data TriggerForm = On Int | Off Int | CC Int | SR Int deriving (Show, Ord, Eq)

data Action a = Action { runA :: TriggerEvent -> Trigger a -> IO () }

type Command a = (TriggerForm, Action a)

data Trigger a = EmptyTrigger |
                      Trigger {
                        dest :: (OscShape,UDP),
                        stack :: MVar ([T.Pattern a]),
                        mapping :: Map.Map TriggerForm (Action a),
                        cycleResolution :: Integer
                        }
-- midi only
midiReader :: PM.PMStream -> IO ([TriggerEvent])
midiReader dev = do
  ee <- PM.readEvents dev
  case ee of
    Right PM.NoError -> return []
    Left evts -> do
      let tevts = handleEvents evts
      case tevts of
        Just e -> return e
        Nothing -> return []

midi2norm :: (Integral a, Fractional b) => a -> b
midi2norm a = (fromIntegral a) / 127

parseEvent x = (s, m, key', val')
  where m = PM.message x
        s = PM.status m
        key' = fromIntegral $ PM.data1 m
        val' = fromIntegral $ PM.data2 m


handleEvent' (0x90, m, key', 0) = handleEvent' (0x80, m, key', 0)
handleEvent' (0x90, m, key', val') = TriggerOn key' val'
handleEvent' (0x80, m, key', val') = TriggerOff key' val'
handleEvent' (cc, m, key', val') = CCChange key' val'

handleEvent x = handleEvent' $ parseEvent x

handleEvents [] = Nothing
handleEvents x = Just $ handleEvents' x

handleEvents' [x] = [handleEvent x]
handleEvents' (x:xs) = (handleEvents' [x]) ++ (handleEvents' xs)



toForm TriggerOff { key=key' } = Off key'
toForm TriggerOn { key=key' } = On key'
toForm CCChange { key=key' } = CC key'
toForm Serial { key=key' } = SR key'
-- generics

handleKey trig e = do
  let mapping' = mapping trig
      form = toForm e
  case Map.member form mapping' of
    True -> do
      let f = (runA (mapping' Map.! form))
      f e trig
      return trig
    False -> do
      return trig


tickPattern stream shape iopattern vel = do
  pattern <- iopattern
  tickPattern' stream shape pattern vel

tickPattern' stream shape pattern vel = do
  let pattern' = pattern -- |+| ((tvel shape) vel)
  now <- getCurrentTime
  let tempo = T.Tempo now 0 0.125
  tOnTick stream shape pattern tempo 0 1 -- len' -- if the pattern is currently triggered, directly evaluate its trigger to update the playing pattern

tickPatternAt trig stream shape pattern tick = do
  now <- getCurrentTime
  let tempo = T.Tempo now (fromIntegral tick) 0.125
  tOnTickAt trig stream shape pattern tempo tick

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

--toMessage :: UDP -> OscShape -> Tempo -> Int -> (Double, OscMap) -> Maybe (IO ())
toMessageAt s shape change tpc tick (o, m) =
  do m' <- applyShape' shape m
     let cycleD = ((fromIntegral tick) / (fromIntegral tpc)) :: Double
         logicalNow = (T.logicalTime change cycleD)
         logicalPeriod = (T.logicalTime change (cycleD + (1/(fromIntegral tpc)))) - logicalNow
         logicalOnset = logicalNow + (logicalPeriod * o) + (latency shape) + nudge
         sec = floor logicalOnset
         usec = floor $ 1000000 * (logicalOnset - (fromIntegral sec))
         oscdata = cpsPrefix ++ preamble shape ++ (parameterise $ catMaybes $ mapMaybe (\x -> Map.lookup x m') (params shape))
         oscdata' = ((int32 sec):(int32 usec):oscdata)
         osc | timestamp shape == BundleStamp = sendOSC s $ Bundle (ut_to_ntpr logicalOnset) [Message (path shape) oscdata]
             | timestamp shape == MessageStamp = sendOSC s $ Message (path shape) oscdata'
             | otherwise = doAt logicalOnset $ sendOSC s $ Message (path shape) oscdata
     return osc
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

tOnTickAt trig s shape pattern change ticks
  = do
       let ticks' = (fromIntegral ticks) :: Integer
           tpc = cycleResolution trig
           a = ticks' T.% tpc
           b = (ticks' + 1) T.% tpc
           ons = (T.seqToRelOnsets (a, b) pattern)
           messages = mapMaybe
                      (toMessageAt s shape change tpc ticks)
                      ons
       E.catch (sequence_ messages) (\msg -> putStrLn $ "oops " ++ show (msg :: E.SomeException))
       return ()


serialIn port = do
  hOpenSerial port defaultSerialSettings

handleSerialEvent str = Serial 1 ((read str) :: Int)

serialReader dev = do
  e <- tryJust (guard . isEOFError) (hGetLine dev)
  return $ either (const []) ((replicate 1).handleSerialEvent) e



sampleproxy latency midi serial streams' = do
  emdev <- midiIn midi
  case emdev of
    Nothing -> do
      return EmptyTrigger -- not cool, since we usually expect a list of channelReplacer
    Just dev -> do
      sdev <- serialIn serial
      sampleproxy' latency ([midiReader dev, serialReader sdev]) streams'

sampleproxy' latency reader stream' = do
  stack' <- newMVar []
  (cps, getNow) <- T.cpsUtils
  let pads = zip (map On [68..83]) (map ((Action).triggerSample)[
        "bd", "btq2", "acs", "acs:2",
        "figf2:2", "mrbs", "tphigh", "crs:2",
        "click2", "clak", "bs", "sclq",
        "fmouse", "fbrd", "fold", "train"])
      mapping' = Map.fromList $ concat [pads, [
                                           (On 90, Action playStack),
                                           (On 91, Action pushRest),
                                           (SR 1, Action playSlice)
                                           ]]
      trig = Trigger stream' stack' mapping' 20
  forkIO $ loop reader trig
  return trig
    where loop reader trig = do
            trig' <- act trig
            threadDelay latency
            loop reader trig'
          act trig = do
            events <- liftM concat $ sequence reader
            foldM handleKey trig events



-- 1. step
-- trigger samples on any drum pad
triggerSample sample e trig = do
  let vol = midi2norm $ val e
      (shape, stream') = dest trig
      pattern = (T.sound (T.p sample) |+| T.gain (T.p $ show vol))
  tickPattern' stream' shape pattern 0
  pushStack pattern trig
  return ()

-- push to pattern stack


pushStack pattern trig = do
  let stackM = stack trig
  stack' <- readMVar stackM
  swapMVar stackM (concat [stack', [pattern]])
  return ()

-- allow adding rests
pushRest e trig = do
  pushStack (T.sound $ T.p "~") trig
  return ()

-- use play button to pop stack and play pattern
popStack trig = do
  let stackM = stack trig
  swapMVar stackM []

playStack e trig = do
  let (shape, stream') = dest trig
  stack' <- popStack trig
  let pattern = T.cat stack'
  tickPattern' stream' shape pattern 0
  return ()


-- What is time? Time is standing still until you move it!
-- step a
-- tickpattern on rotary location call
readStack trig = do
  let stackM = stack trig
  readMVar stackM

playSlice e trig = do
  let (shape, stream') = dest trig
      val' = (val e)
  stack' <- readStack trig
  tickPatternAt trig stream' shape (T.cat stack') $ val'

-- step b
-- allow multiple input types triggers (hook up arduino or other midi controller to control "time" via rotary, to avoid skipping)

{-
 step b.1

  serialIn "/dev/tty.usbserial"
  midiIn "QUNEO"
  oscIn "127.0.0.1" 7771

-}


-- 2. step
-- allow holding play button to repeat pattern
-- allow adjusting speed by slider
-- allow entering braces mode (e.g. for [], {}, or () in case of poly rythms) while holding another (non drumpad) button
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
