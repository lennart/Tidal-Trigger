module Sound.Tidal.Trigger.Context where

import qualified Data.Map.Strict as Map
import Control.Monad
import Data.Maybe
import Control.Concurrent

import Sound.Tidal.Trigger.Stream
import Sound.Tidal.Trigger.Types
import Sound.Tidal.Trigger.Actions


toForm TriggerOff { key=key' } = Off key'
toForm TriggerOn { key=key' } = On key'
toForm CCChange { key=key' } = CC key'
toForm Serial { key=key' } = SR key'

handleKey trig e = do
  let mapping' = mapping trig
      form = toForm e
  case Map.member form mapping' of
    True -> do
      let f = (runA (mapping' Map.! form))
      trig' <- f e trig
      return trig'
    False -> do
      return trig



triggerproxy latency cycleres mapping inputReaders output = do
  readers <- sequence inputReaders
  triggerproxy' latency cycleres mapping readers output

triggerproxy' latency cycleres mapping readers output = do
  stack' <- newMVar []
  let trig = Trigger output stack' mapping cycleres [] 0
  forkIO $ loop readers trig
  return trig
    where loop readers trig = do
            trig' <- act trig
            threadDelay latency
            loop readers trig'
          act trig = do
            events <- liftM concat $ sequence readers
            foldM handleKey trig events
