module Sound.Tidal.Trigger.Responder where

import Data.Maybe
import Control.Concurrent

import Sound.OSC.FD


import qualified Sound.PortMidi as PM

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
