{-# LANGUAGE FlexibleInstances #-}
module Sound.Tidal.Trigger.Types where

import qualified Data.Map.Strict as Map
import Sound.OSC.FD
import Data.Ratio
import qualified Sound.Tidal.Context as T
import Control.Concurrent


-- data TriggerEvent = TriggerOn { path :: Int, val :: [] } |
--                     TriggerOff { key :: Int, val :: Int } |
--                     CCChange { key :: Int, val :: Int} |
--                     Serial { key :: Int, val :: Int }
--                   deriving (Show)

data Input = IVInt { i_int :: Int } | IVDouble { i_double :: Double } | IVString { i_string :: String } deriving (Eq, Show, Read)


class InputValue a where
  i_put :: a -> Input
  i_get :: Input -> Maybe a

instance InputValue Int where
  i_put = IVInt
  i_get i = case i of {IVInt x -> Just x; _ -> Nothing }

instance InputValue Double where
  i_put = IVDouble
  i_get i = case i of {IVDouble x -> Just x; _ -> Nothing }

instance InputValue [Char] where
  i_put = IVString
  i_get i = case i of {IVString x -> Just x; _ -> Nothing }

--data TriggerValue = Int | String | Double deriving (Show)

data TriggerEvent = TriggerEvent String Input deriving (Show)

--handle rig e =



type ActionArgs = ([Variant], Material)

newtype Action = Action { runA :: TriggerEvent -> ActionArgs }



                 -- | Action2 { runB :: b -> a } | Action2b { runC :: c -> a } | Action2c { runD :: d -> a }


                -- TriggerEvent -> Trigger a -> IO (Trigger a) }

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



--toMarker m = (path m, args m)

data Arg = Arg

  --Int | String | Double | Ratio

type Path = String

type Variant = (Path, [Arg])



data Context = C [Action]


newtype Rig a = Rig { unplug :: a }

type Material = ([String], [Int], [Rational])




--data X = X a

--newtype

data Trigger = EmptyTrigger |
                      Trigger {
                        dest :: (T.OscShape,UDP),
                        stack :: [String],
                        vstack :: [Double],
                        playhead :: Int,
                        dir :: Direction,
                        mapping :: Map.Map TriggerForm Action,
                        cycleResolution :: Int,
                        fifo :: [Char],
                        pick :: Int,
                        tempo :: Double
                        }
