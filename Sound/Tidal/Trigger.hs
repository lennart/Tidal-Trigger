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
data TriggerShape a b = EmptyTShape |
                      TShape {
                        streams :: [(OscShape,UDP)],
                        patternStatesM :: [MVar (Bool)],
                        patternsM :: [MVar (IO (Pattern a))],
                        knobs :: [MVar (Int)],
                        sampler :: [IO (Pattern a) -> IO ()], -- sm1 - sm8
                        ostream :: UDP,
                        tvel :: MVar (Double -> Pattern a)
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
getDevices = do
  PM.initialize
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

handleNoteOn (sshape, stream) patternStateM patternM key' val' = do
  maybeP <- tryReadMVar patternM
  case maybeP of
    Nothing ->
      return ()
    Just pattern -> do
      forkIO $ do
        swapMVar patternStateM True
        tickPattern stream sshape pattern (normMIDIRange val')
      return ()

handleNoteOff (sshape, stream) patternStateM patternM key' val' = do
  forkIO $ do
    swapMVar patternStateM False
    tickPattern stream sshape makeSilence (normMIDIRange val')
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
    _ -> return () --putStrLn ("Received out of bounds note on Msg " ++ (show key'') ++ "/" ++ (show len))

handleCC ccroot shape key' val' = do
  let key'' = keyToIndex ccroot key'
      recordCCRoot = 90
      knobs' = knobs shape
  case key' of
    x | (keyToIndex ccroot x) < (length knobs') && (keyToIndex ccroot x) >= 0 -> do
      let knob = (knobs' !! (keyToIndex ccroot x))
      forkIO $ f knob val'
      return ()
        where f knob val' = do
                swapMVar knob val'
                return ()
    x | (keyToIndex recordCCRoot x) >= 0 && (keyToIndex recordCCRoot x) < 8 -> do
      case (keyToIndex recordCCRoot x) of
        0 -> do
          forkIO $ do
            case val' of
              0 -> do
                sendOSC (ostream shape) $ Message "/set_current_loop" [(int32 (keyToIndex recordCCRoot x))]
                sendOSC (ostream shape) $ Message "/pause_input" [(int32 1)]
              _ -> do
                sendOSC (ostream shape) $ Message "/pause_input" [(int32 0)]
        1 -> do
          forkIO $ do
            case val' of
              0 -> do
                return ()
              _ -> do
                sendOSC (ostream shape) $ Message "/clear_loop" [(int32 1)]
        2 -> do
          forkIO $ do
            sendOSC (ostream shape) $ Message "/set_current_speed" [(Sound.OSC.FD.float (normMIDIRange val'))]



      return ()
    _ -> return () -- putStrLn ("Received out of bounds CC Msg" ++ (show key''))


handleKey ccroot shape (TriggerOn key' val') = handleNote ccroot shape key' val' handleNoteOn
handleKey ccroot shape (TriggerOff key' val') = handleNote ccroot shape key' val' handleNoteOff
handleKey ccroot shape (CCChange key' val') = handleCC ccroot shape key' val'

handleKeys'' ccroot shape [x] = [handleKey ccroot shape x]
handleKeys'' ccroot shape (x:xs) = (handleKeys'' ccroot shape [x]) ++ (handleKeys'' ccroot shape xs)

handleKeys' ccroot shape [] = do return ()
handleKeys' ccroot shape x = sequence_ $ handleKeys'' ccroot shape x


handleKeys ccroot shape Nothing = do return ()
handleKeys ccroot shape (Just x) = handleKeys' ccroot shape x


tickPattern stream shape iopattern vel = do
  pattern <- iopattern
  let pattern' = pattern -- |+| ((tvel shape) vel)
  now <- getCurrentTime
  let tempo = Tempo now 0 0.125
  -- interpret any velocity below 10 as 10 so tOnTick will play sometjhing
  -- let len' = case len of
  --       x | x >= (10%127) -> x
  --       _ -> (10%127)
  tOnTick stream shape pattern tempo 0 1 -- len' -- if the pattern is currently triggered, directly evaluate its trigger to update the playing pattern


handlePatternUpdate (sshape, stream) pattern True = do
  tickPattern stream sshape pattern 1
  return True

handlePatternUpdate stream pattern False =
  return False

updatePattern stream patternStateM patternM pattern = do
  state <- readMVar patternStateM
  state' <- handlePatternUpdate stream pattern state
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


readKnobM m = tryReadMVar m

readKnob m = readKnobM m

normMIDIRange :: (Integral a, Fractional b) => a -> b
normMIDIRange a = (fromIntegral a) / 127

sound' pattern = do
  return $ sound pattern

makeKnobM knob p' = do v <- readKnob knob
                       let v' = maybeListToPat [midiConvert <$> v]
                       return $ p' v'

makeKnob knob = (\p' -> makeKnobM knob p') :: MIDIValue a => (Pattern a -> Pattern b) -> IO (Pattern b)


k :: MIDIValue b => MVar (Int) -> (Pattern b -> Pattern a) -> IO (Pattern a)
k knob synth_param = makeKnobM knob synth_param

class MIDIValue a where
  midiConvert :: Int -> a

instance MIDIValue Int where
  midiConvert v = v

instance MIDIValue Double where
  midiConvert v = normMIDIRange v




soundAndKnob pattern knobParam = do k' <- knobParam
                                    s' <- sound' pattern
                                    let p' = s' |+| k'
                                    return $ p'

playhead knob ratio = (<*>) (playhead' knob ratio)

playhead' knob ratio = do v <- readKnob knob
                          case v of
                            Nothing -> return $ (~>) 0
                            Just v' -> do
                              let v'' = (ratio *) $ fromIntegral v'
                              return $ (~>) v''

mergeIO :: IO (Pattern OscMap) -> IO (Pattern OscMap) -> IO (Pattern OscMap)
mergeIO p1 p2 = do p1' <- p1
                   p2' <- p2
                   return $ p1' |+| p2'



infixl 1 |++|
(|++|) :: IO (Pattern OscMap) -> IO (Pattern OscMap) -> IO (Pattern OscMap)
(|++|) = mergeIO


midiOut name = do
  edev <- getIDForOutputDeviceName name
  case edev of
    Nothing -> do error ("Device '" ++ show name ++ "' not found")
                  putStrLn "List of Available Output Device Names"
                  putStrLn =<< displayOutputDevices
                  return Nothing
    Just deviceID -> do
      PM.initialize
      result <- PM.openOutput deviceID 1
      case result of
        Right err ->
          do
            error ("Failed opening MIDI Output Port: " ++ (show deviceID) ++ ": " ++ (show name) ++ "\nError: " ++ show err)
            return Nothing
        Left dev -> return $ Just dev

midiIn name = do
  edev <- getIDForInputDeviceName name
  case edev of
    Nothing -> do error ("Device '" ++ show name ++ "' not found")
                  putStrLn "List of Available Input Device Names"
                  putStrLn =<< displayInputDevices
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

sampleproxy latency iname oname ccroot streams' = do
  edev <- midiIn iname
  case edev of
    Nothing -> do
      return EmptyTShape -- not cool, since we usually expect a list of channelReplacer
    Just dev -> do
      eodev <- midiOut oname
      case eodev of
        Nothing -> do
          return EmptyTShape
        Just odev -> do
          sampleproxy' latency dev odev ccroot streams'

makeSilence :: IO (Pattern a)
makeSilence = do return silence

padState percent len index
  | percent > pos = 0
  | otherwise = 127
   where pos = floor $ (* 100) $ ((/) (fromIntegral index) (fromIntegral len))

mapPercent2Midi x = floor ((1.27) * x)

handleTidalResponse conn shape m = do
  let ccroot = 90
  case m of
    Just (Message "/recording/heartbeat" (percent:index:rest)) -> do
      -- apply recvd percentage to 4x2 pad matrix
      time <- PM.time
      let percent' = (fromJust $ d_get percent) :: Int
          states = map (padState percent' 8) [0..7]
--          msgs = zipWith ($) (zipWith ($) (map (PM.PMMsg) (replicate 8 0xB0)) (map ((+) ccroot) [0..7])) states
  --        evts = zipWith ($) (map PM.PMEvent msgs) (replicate 8 time)
          evts = [PM.PMEvent (PM.PMMsg 0xB0 0x06 (mapPercent2Midi (fromIntegral percent'))) time]
      PM.writeEvents conn evts
      return ()
    Just (Message "/loop/current" (index:rest)) -> do
      -- turn off all lights
      time <- PM.time
      let index' = (fromJust $ d_get index) :: Int
          states = replicate 8 0
          msgs = zipWith ($) (zipWith ($) (map (PM.PMMsg) (replicate 8 0xB0)) (map ((+) ccroot) [0..7])) states
          evts = zipWith ($) (map PM.PMEvent msgs) (replicate 8 time)
          evts' = evts ++ [PM.PMEvent (PM.PMMsg 0xB0 (ccroot + (fromIntegral index')) 127) time]
      PM.writeEvents conn evts'
      return ()
    Just (Message "/recording/loop" (index:rest)) -> do
      -- turn off all lights
      time <- PM.time
      let index' = (fromJust $ d_get index) :: Int
          states = replicate 8 0
          msgs = zipWith ($) (zipWith ($) (map (PM.PMMsg) (replicate 8 0xB0)) (map ((+) ccroot) [0..7])) states
          evts = zipWith ($) (map PM.PMEvent msgs) (replicate 8 time)
          evts' = evts ++ [PM.PMEvent (PM.PMMsg 0xB0 (ccroot + (fromIntegral index')) 127) time]
      PM.writeEvents conn evts'
      return ()
    Just (Message "/paused/loop" (index:rest)) -> do
      -- turn off all lights
      time <- PM.time
      let index' = (fromJust $ d_get index) :: Int
          states = replicate 8 0
          msgs = zipWith ($) (zipWith ($) (map (PM.PMMsg) (replicate 8 0xB0)) (map ((+) ccroot) [0..7])) states
          evts = zipWith ($) (map PM.PMEvent msgs) (replicate 8 time)
          evts' = evts ++ [PM.PMEvent (PM.PMMsg 0xB0 (ccroot + (fromIntegral index')) 127) time]
      PM.writeEvents conn evts'
      return ()
    x -> do
      putStrLn ("[tidal-trigger] unknown message received: " ++ (show m))
      return ()


tidalResponder conn shape = do
  x <- udpServer "127.0.0.1" 6010
  putStrLn ("[tidal-trigger] starting dirt osc responder")
  forkIO $ loop x conn shape
  return ()
    where loop x conn shape =
            do m <- recvMessage x
               handleTidalResponse conn shape m
               loop x conn shape


sampleproxy' latency conn oconn ccroot streams' = do
--  streams' <- replicateM n_chans $ openUDP "127.0.0.1" 7771
  patternsM' <- replicateM 8 (newMVar makeSilence)
  patternStatesM' <- replicateM 8 (newMVar False)
  tvel' <- newMVar (\x -> silence)
  knobs' <- replicateM 8 (newMVar 0)
  ostream' <- (openUDP "127.0.0.1" 7771)
  (cps, getNow) <- cpsUtils
  let channelReplacer = zipWith ($) (zipWith ($) (map updatePattern streams') patternStatesM') patternsM'
      shape = TShape streams' patternStatesM' patternsM' knobs' channelReplacer ostream' tvel'
  tidalResponder oconn shape
  forkIO $ loop conn ccroot shape
  return shape
    where loop conn ccroot shape = do v <- readCtrl conn
                                      handleKeys ccroot shape v
                                      threadDelay latency
                                      loop conn ccroot shape
