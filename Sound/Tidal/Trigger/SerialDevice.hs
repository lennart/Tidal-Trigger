module Sound.Tidal.Trigger.SerialDevice where

import System.IO
import System.IO.Error
import System.Hardware.Serialport
import Control.Exception (tryJust)
import Control.Monad (guard)

import Sound.Tidal.Trigger.Types

serialIn port = do
  hOpenSerial port defaultSerialSettings

handleSerialEvent str = zipWith Serial [1..] ((map read $ words str) :: [Int])

serialReader dev = do
  e <- tryJust (guard . isEOFError) (hGetLine dev)
  return $ either (const []) (handleSerialEvent) e
