module Sound.Tidal.Trigger.Types where

import qualified Data.Map.Strict as Map
import Sound.OSC.FD
import qualified Sound.Tidal.Context as T
import Control.Concurrent


data TriggerEvent = TriggerOn { key :: Int, val :: Int } |
                    TriggerOff { key :: Int, val :: Int } |
                    CCChange { key :: Int, val :: Int} |
                    Serial { key :: Int, val :: Int }
                  deriving (Show)

data Action a = Action { runA :: TriggerEvent -> Trigger a -> IO () }

data TriggerForm = On Int | Off Int | CC Int | SR Int deriving (Show, Ord, Eq)

data Trigger a = EmptyTrigger |
                      Trigger {
                        dest :: (T.OscShape,UDP),
                        stack :: MVar ([T.Pattern a]),
                        mapping :: Map.Map TriggerForm (Action a),
                        cycleResolution :: Integer
                        }
