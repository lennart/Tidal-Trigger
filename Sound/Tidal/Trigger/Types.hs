{-# LANGUAGE FlexibleInstances #-}
module Sound.Tidal.Trigger.Types where

import qualified Data.Map.Strict as Map
import Sound.OSC.FD
import Data.Ratio
import qualified Sound.Tidal.Context as T
import Control.Concurrent

data Input = IVInteger Integer | IVInt Int | IVDouble Double | IVString String deriving (Eq, Show, Read)

class (Read a) => InputValue a where
  i_put :: a -> Input
  i_get :: Input -> Maybe a

instance InputValue Int where
  i_put = IVInt
  i_get i = case i of {IVInt x -> Just x; _ -> Nothing }

instance InputValue Integer where
  i_put = IVInteger
  i_get i = case i of {IVInteger x -> Just x; _ -> Nothing }

instance InputValue Double where
  i_put = IVDouble
  i_get i = case i of {IVDouble x -> Just x; _ -> Nothing }

instance InputValue [Char] where
  i_put = IVString
  i_get i = case i of {IVString x -> Just x; _ -> Nothing }

-- instance Read a => Read (InputValue a) where
--   readsPrec d r

type Path = String

data TriggerEvent = TriggerEvent { tpath :: Path, tinput :: Input } deriving (Show)

data Direction = CW | CCW | S deriving (Show, Eq, Ord)

instance Enum Direction where
  toEnum (-1) = CCW
  toEnum 1 = CW
  toEnum 0 = S
  toEnum x = error ("Unable to represent " ++ (show x) ++ "as Direction")

  fromEnum CCW = -1
  fromEnum CW = 1
  fromEnum S = 0

type KeyMap m k v = m (Map.Map k v)

data Rig m k v = Rig { state :: MVar (KeyMap m k v, [KeyMap m k v]),
                   merge :: KeyMap m k v -> KeyMap m k v -> KeyMap m k v,
                   welder :: Input -> KeyMap m k v -> [(k, Input -> KeyMap m k v)],
                   transformers :: [Input -> Input]
                 }

type ActionMap m k v = Map.Map (Path, k) (k, Input -> KeyMap m k v)

data Performance m k v = Perf {
  rig :: Rig m k v,
  actions :: ActionMap m k v, -- Map.Map (Path, k) (k, Input -> KeyMap m k v), -- combination of Path and Param is unique
  tempo :: Double,
  sockets :: [(Input -> KeyMap m k v)],
  pending :: [TriggerEvent]
  }
