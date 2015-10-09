module Sound.Tidal.Trigger.SerialDevice where

import System.IO
import System.IO.Error
import System.Hardware.Serialport
import Control.Exception (tryJust)
import Control.Monad (guard)

import Sound.Tidal.Trigger.Parse
import Sound.Tidal.Trigger.Types

serialIn port = do
  hOpenSerial port defaultSerialSettings

handleSerialEvent str = zipWith TriggerEvent (map (((++) "serial:").show) [1..]) (parseInput str)

serialReader dev = do
  e <- tryJust (guard . isEOFError) (hGetLine dev)
  return $ either (const []) (handleSerialEvent) e
