module Sound.Tidal.Trigger.Device where

import qualified Sound.PortMidi as PM


--withPortMidi = bracket_ PM.initialize PM.terminate

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


-- opening in/out

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