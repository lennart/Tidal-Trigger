module Sound.Tidal.Trigger.Context (
  trigger
                                   ) where

import qualified Data.Map.Strict as Map
import Control.Monad
import Data.Maybe
import Control.Concurrent

import Sound.Tidal.Trigger.Stream
import Sound.Tidal.Trigger.Types
import Sound.Tidal.Trigger.Actions


-- coding for what is otherwise annoying on hardware

{-

--pads ""
-}


-- toForm TriggerOff { key=key' } = Off key'
-- toForm TriggerOn { key=key' } = On key'
-- toForm CCChange { key=key' } = CC key'
-- toForm Serial { key=key' } = SR key'

handleKey rig e =
  rig
  -- bool (id) (handle rig e) connected $ rig

  --   True -> do
  --     let f = (runA (mapping' Map.! form))
  --     trig' <- f e rig
  --     return trig'
  --   False -> do
  --     return trig
  --   where
  --     mapping' = mapping trig
  --     isMatch = Map.member form mapping'
  --     form = toForm e




trigger latency rig inputReaders output = do
  readers <- sequence inputReaders
  trigger' latency rig readers output


trigger' latency rig readers output = do
  forkIO $ loop rig readers
  return rig
    where loop rig readers = do
            rig' <- act rig
            threadDelay latency
            loop rig' readers
          act rig = do
            events <- liftM concat $ sequence readers
            return $ foldl handleKey rig events
