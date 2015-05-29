module Sound.Tidal.Trigger where

import Sound.Tidal.Context
import qualified Sound.PortMidi as PM

import Control.Monad
import Control.Exception as E
import Foreign.C
import Data.List
import Data.Maybe
import Data.Time

import Sound.OSC.FD
import Sound.Tidal.Stream

import System.IO.Unsafe

data TriggerEvent = TriggerOn { key :: Int, val :: Int } | TriggerOff { key :: Int, val :: Int } | CCChange { key :: Int, val :: Int} deriving (Show)
data TriggerShape a = EmptyTShape |
                      TShape {
                        streams :: [UDP],
                        patternStatesM :: [MVar (Bool)],
                        patternsM :: [MVar (IO (Pattern a))],
                        knobs :: [MVar (Int)],
                        knobsHandler :: [(Pattern Int -> Pattern a) -> IO (Pattern a)], -- knob1 - knob8
                        sampler :: [IO (Pattern a) -> IO ()] -- sm1 - sm8
                        }


withPortMidi = bracket_ PM.initialize PM.terminate

displayOutputDevices = do
  devices <- getIndexedDevices
  return $ displayDevices $ getOutputDevices devices

displayInputDevices = do
  devices <- getIndexedDevices
  return $ displayDevices $ getInputDevices devices

displayDevices :: Show a => [(a, PM.DeviceInfo)] -> String
displayDevices devices =
  let indices = map (show . fst) devices
      names = map ((":\t"++) . PM.name . snd) devices
      pairs = zipWith (++) indices names
  in unlines (["ID:\tName"]++pairs)

getOutputDevices = filter (PM.output . snd)
getInputDevices = filter (PM.input . snd)

getIndexedDevices = do
  rawDevices <- getDevices
  return $ zip [0..] rawDevices

getDevices :: IO ([PM.DeviceInfo])
getDevices = withPortMidi $ do
  count <- PM.countDevices
  mapM PM.getDeviceInfo [0..(count - 1)]


getIDForOutputDeviceName name = do
  odevs <- fmap getOutputDevices getIndexedDevices
  let res = filter (\n -> (PM.name . snd) n == name) odevs
  case res of
    [] -> return Nothing
    [dev] -> return $ Just $ fromIntegral $ fst dev

getIDForInputDeviceName name = do
  devs <- fmap getInputDevices getIndexedDevices
  let res = filter (\n -> (PM.name . snd) n == name) devs
  case res of
    [] -> return Nothing
    [dev] -> return $ Just $ fromIntegral $ fst dev


runnow g d p' = do now <- g
                   d $ (now + 0.1) ~> p'

oneshot g d p' = runnow g d $ seqP [(0, 1, p')]

oneshot' g d n p' = runnow g d $ seqP [(0, n, p')]

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

readCtrl dev = do
  ee <- PM.readEvents dev
  case ee of
    Right PM.NoError -> return Nothing
    Left evts -> return $ handleEvents evts

handleNoteOn stream patternStateM patternM key' val' = do
  maybeP <- tryReadMVar patternM
  case maybeP of
    Nothing ->
      return ()
    Just pattern -> do
      forkIO $ do
        swapMVar patternStateM True
        tickPattern stream dirt pattern (normMIDIRange val')
      return ()

handleNoteOff stream patternStateM patternM key' val' = do
  forkIO $ do
    swapMVar patternStateM False
    tickPattern stream dirt makeSilence (normMIDIRange val')
  return ()

keyToIndex ccroot key' = (fromIntegral key') - ccroot

handleNote ccroot shape key' val' f = do
  let key'' = keyToIndex ccroot key'
      streams' = streams shape
      len = length streams'
  case key'' of
    x | x < len && x >= 0 -> do
      let patternM = (patternsM shape) !! x
          patternStateM = (patternStatesM shape) !! x
      f (streams' !! x) patternStateM patternM key'' val'
      return ()
    _ -> putStrLn ("Received out of bounds note on Msg " ++ (show key'') ++ "/" ++ (show len))

handleCC ccroot shape key' val' = do
  let key'' = keyToIndex ccroot key'
      knobs' = knobs shape
  case key'' of
    x | x < (length knobs') && x >= 0 -> do
      let knob = (knobs' !! x)
      forkIO $ f knob val'
      return ()
        where f knob val' = do
                swapMVar knob val'
                return ()
    _ -> putStrLn ("Received out of bounds CC Msg" ++ (show key''))


handleKey ccroot shape (TriggerOn key' val') = handleNote ccroot shape key' val' handleNoteOn
handleKey ccroot shape (TriggerOff key' val') = handleNote ccroot shape key' val' handleNoteOff
handleKey ccroot shape (CCChange key' val') = handleCC ccroot shape key' val'

handleKeys'' ccroot shape [x] = [handleKey ccroot shape x]
handleKeys'' ccroot shape (x:xs) = (handleKeys'' ccroot shape [x]) ++ (handleKeys'' ccroot shape xs)

handleKeys' ccroot shape [] = do return ()
handleKeys' ccroot shape x = sequence_ $ handleKeys'' ccroot shape x


handleKeys ccroot shape Nothing = do return ()
handleKeys ccroot shape (Just x) = handleKeys' ccroot shape x


tickPattern stream shape iopattern len = do
  pattern <- iopattern
  now <- getCurrentTime
  let tempo = Tempo now 0 0.5
  tOnTick stream shape pattern tempo 0 len -- if the pattern is currently triggered, directly evaluate its trigger to update the playing pattern


handlePatternUpdate stream pattern True = do
  tickPattern stream dirt pattern 1
  return True

handlePatternUpdate stream pattern False =
  return False

updatePattern stream patternStateM patternM pattern = do
  state <- readMVar patternStateM
  state' <- handlePatternUpdate stream pattern state
--  swapMVar knobPatternM knobPatterns
  swapMVar patternM pattern
  return ()

tStart :: String -> Int -> OscShape -> IO (MVar (OscPattern))
tStart address port shape
  = do patternM <- newMVar silence
       s <- openUDP address port
       let ot = (onTick s shape patternM) :: Tempo -> Int -> IO ()
       forkIO $ clockedTick ticksPerCycle ot
       return patternM


--tOnTick :: UDP -> OscShape -> MVar (OscPattern) -> Tempo -> Int -> IO ()
tOnTick s shape pattern change ticks len
  = do
       let ticks' = (fromIntegral ticks) :: Integer
           a = ticks' % ticksPerCycle
           b = (ticks' + 1) % ticksPerCycle
           messages = mapMaybe
                      (toMessage s shape change ticks)
                      (seqToRelOnsets (0, len) pattern)
       E.catch (sequence_ messages) (\msg -> putStrLn $ "oops " ++ show (msg :: E.SomeException))
       return ()


--samplerstream :: String -> Int -> OscShape -> IO (OscPattern -> IO ())
samplerstream address port shape =
  openUDP address port
  -- = do patternM <- start address port shape
  --      return $ \p -> do swapMVar patternM p
  --                        return ()

--readKnobM :: MVar (Int) -> IO (Maybe Int)
readKnobM m = tryReadMVar m

--readKnob :: MVar (Int) -> Maybe Int
readKnob m = readKnobM m

normMIDIRange :: (Integral a, Fractional b) => a -> b
normMIDIRange a = (fromIntegral a) / 127

--knobPattern :: MVar (Int) -> Pattern Double
--knobPattern m = maybeListToPat [normMIDIRange <$> readKnob m]

--knobValue :: MVar (Int) -> Rational
-- knobValue m = case normMIDIRange <$> readKnob m of
--                 Just i -> toRational i
--                 Nothing -> 0.0

--kv = knobValue
--kr = knobPattern

sound' pattern = do
  return $ sound pattern

makeKnobM knob p' = do v <- readKnob knob
                       let v' = maybeListToPat [v]
                       return $ p' v'

makeKnob knob = (\p' -> makeKnobM knob p') :: (Pattern Int -> Pattern a) -> IO (Pattern a)

soundAndKnob pattern knobParam = do k' <- knobParam
                                    s' <- sound' pattern
                                    let p' = s' |+| k'
                                    return $ p'

midiIn name = do
  edev <- getIDForInputDeviceName name
  case edev of
    Nothing -> do error ("Device '" ++ show name ++ "' not found")
                  return Nothing
    Just deviceID -> do
      PM.initialize
      result <- PM.openInput deviceID
      case result of
        Right err ->
          do
            error ("Failed opening MIDI Input Port: " ++ (show deviceID) ++ ": " ++ (show name) ++ "\nError: " ++ show err)
            return Nothing
        Left dev -> return $ Just dev

sampleproxy latency name ccroot = do
  edev <- midiIn name
  case edev of
    Nothing -> do
      putStrLn "List of Available Input Device Names"
      putStrLn =<< displayInputDevices
      return EmptyTShape -- not cool, since we usually expect a list of channelReplacer
    Just dev -> do
      sampleproxy' latency dev ccroot

makeSilence :: IO (Pattern a)
makeSilence = do return silence

sampleproxy' latency conn ccroot = do
  let n_chans = 8
  streams' <- replicateM n_chans $ openUDP "127.0.0.1" 7771
  patternsM' <- replicateM 8 (newMVar makeSilence)
  patternStatesM' <- replicateM 8 (newMVar False)
  knobs' <- replicateM 8 (newMVar 0)
  let knobsHandler' = map (makeKnob) knobs'
--  knobPatternsM' <- replicateM 8 (newMVar [])
  (cps, getNow) <- cpsUtils
  let channelReplacer = zipWith ($) (zipWith ($) (map updatePattern streams') patternStatesM') patternsM'
      shape = TShape streams' patternStatesM' patternsM' knobs' knobsHandler' channelReplacer
  forkIO $ loop conn ccroot shape
  return shape
    where loop conn ccroot shape = do v <- readCtrl conn
                                      handleKeys ccroot shape v
                                      threadDelay latency
                                      loop conn ccroot shape
