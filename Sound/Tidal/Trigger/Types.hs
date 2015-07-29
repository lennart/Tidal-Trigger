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

data Action a = Action { runA :: TriggerEvent -> Trigger a -> IO (Trigger a) }

data TriggerForm = On Int | Off Int | CC Int | SR Int deriving (Show, Ord, Eq)

data Direction = CW | CCW | S deriving (Show, Eq, Ord)

instance Enum Direction where
  toEnum (-1) = CCW
  toEnum 1 = CW
  toEnum 0 = S
  toEnum x = error ("Unable to represent " ++ (show x) ++ "as Direction")

  fromEnum CCW = -1
  fromEnum CW = 1
  fromEnum S = 0



data Trigger a = EmptyTrigger |
                      Trigger {
                        dest :: (T.OscShape,UDP),
                        stack :: [String],
                        vstack :: [Double],
                        playhead :: Int,
                        dir :: Direction,
                        mapping :: Map.Map TriggerForm (Action a),
                        cycleResolution :: Int,
                        fifo :: [Char],
                        pick :: Int,
                        tempo :: Double
                        }
