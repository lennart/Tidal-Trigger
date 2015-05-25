module Sound.Tidal.Trigger where

import Sound.Tidal.Context
import qualified Sound.PortMidi as PM

import Control.Monad
import Control.Exception
import Foreign.C
import Data.List

import Sound.Tidal.Stream

import System.IO.Unsafe

data TriggerEvent = TriggerOn { key :: Int, val :: Int } | TriggerOff { key :: Int, val :: Int } | CCChange { key :: Int, val :: Int} deriving (Show)
data TriggerShape a = EmptyTShape | TShape { streams :: [Pattern a -> IO ()], patternStatesM :: [MVar (Bool)], patternsM :: [MVar (Pattern a)], knobs :: [MVar (Int)], sampler :: [Pattern a -> IO (Pattern a)] }


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

--readCtrl :: PM.PMStream -> IO (Maybe (CLong, CLong))
readCtrl dev = do
  ee <- PM.readEvents dev
  case ee of
    Right PM.NoError -> return Nothing
    Left evts ->
      do
        case evts of
          [] -> return Nothing
          (x:xs) -> do
            let m = PM.message x
                s = PM.status m
                key' = fromIntegral $ PM.data1 m
                val' = fromIntegral $ PM.data2 m
            case s of
              0x90 -> case val' of
                           0 -> return (Just $ TriggerOff key' val')
                           _ -> return (Just $ TriggerOn key' val')
              0x80 -> return (Just $ TriggerOff key' val')
              _ -> do
                return (Just $ CCChange key' val') -- we assume a CC for anything other than note on/off

handleNoteOn stream patternStateM patternM key' val' = do
  putStrLn ("Play Sample" ++ (show key'))
  maybeP <- tryReadMVar patternM
  case maybeP of
    Nothing ->
      return ()
    Just pattern -> do
      forkIO $ do
        putStrLn ("Swap Sampler Pattern")
        swapMVar patternStateM True
        putStrLn ("Sampler Pattern swapped")
        stream pattern
      return ()

handleNoteOff stream patternStateM key' = do
  putStrLn ("Stop Sample" ++ (show key'))
  forkIO $ do
    swapMVar patternStateM False
    stream silence
  return ()

keyToIndex ccroot key' = (fromIntegral key') - ccroot

handleKeys ccroot shape v = do
  case v of
    Nothing -> return ()
    Just (TriggerOn key' val') -> do
      let key'' = keyToIndex ccroot key'
          streams' = streams shape
      case key'' of
        x | x < (length streams') && x >= 0 -> do
          let patternM = (patternsM shape) !! x
              patternStateM = (patternStatesM shape) !! x
          handleNoteOn (streams' !! x) patternStateM patternM key'' val'
          return ()
        _ -> putStrLn ("Received out of bounds note on Msg " ++ (show key'') ++ "/" ++ (show $ length streams'))
    Just (TriggerOff key' val') -> do
      let key'' = keyToIndex ccroot key'
          streams' = streams shape
      case key'' of
        x | x < (length streams') && x >= 0 -> do
          let stream = streams' !! x
              patternM = (patternsM shape) !! x
              patternStateM = (patternStatesM shape) !! x
          handleNoteOff stream patternStateM key''
          return ()
        _ -> putStrLn ("Received out of bounds noteOff Msg" ++ (show key'))
    Just (CCChange key' val') -> do
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

handlePatternUpdate stream pattern True = do
  putStrLn ("Retrigger running sampler with new Pattern")
  stream pattern -- if the pattern is currently triggered, directly evaluate its trigger to update the playing pattern
  return True

handlePatternUpdate stream pattern False =
  return False

updatePattern stream patternStateM patternM pattern = do
  state <- readMVar patternStateM
  state' <- handlePatternUpdate stream pattern state
  putStrLn ("Updating Pattern")
  swapMVar patternM pattern
  return pattern


readKnobM :: MVar (Int) -> IO (Maybe Int)
readKnobM m = tryReadMVar m

readKnob :: MVar (Int) -> Maybe Int
readKnob m = unsafePerformIO $ readKnobM m

normMIDIRange :: (Integral a, Fractional b) => a -> b
normMIDIRange a = (fromIntegral a) / 127

knobPattern :: MVar (Int) -> Pattern Double
knobPattern m = maybeListToPat [normMIDIRange <$> readKnob m]

knobValue :: MVar (Int) -> Rational
knobValue m = case normMIDIRange <$> readKnob m of
                Just i -> toRational i
                Nothing -> 0.0

kv = knobValue
kr = knobPattern

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

sampleproxy' latency conn ccroot = do
  patternsM' <- replicateM 8 (newMVar silence)
  patternStatesM' <- replicateM 8 (newMVar False)
  knobs' <- replicateM 8 (newMVar 0)
  (cps, getNow) <- cpsUtils
  streams' <- replicateM (length patternsM') dirtStream
  let streams'' = map (runnow getNow) streams'
      channelReplacer = zipWith ($) (zipWith ($) (map updatePattern streams'') patternStatesM') patternsM'
      shape = TShape streams'' patternStatesM' patternsM' knobs' channelReplacer
  forkIO $ loop conn ccroot shape
  return shape
    where loop conn ccroot shape = do v <- readCtrl conn
                                      handleKeys ccroot shape v
                                      threadDelay latency
                                      loop conn ccroot shape
