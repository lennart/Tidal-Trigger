module Sound.Tidal.Trigger.Rigs.Sutherland (sutherland) where

import Sound.Tidal.Trigger.Types
import Sound.Tidal.Trigger.Actions

pads samples notes = zip notes' samples'
  where
    notes' = map On notes
    samples' = map ((Action).triggerSample) samples

-- c x = T.run 16

--rigmap f a b =

--data Rig = Rig

--fmap (\x y -> (On x, (Action).triggerSample y)) ((+68) <$> (c 4 4)) "clak2/crs"

rig samples notes fs = asMapping $ concat [pads samples notes, fs]

sutherland = rig {- [
  "btq3",
  "btq2",
  "acs",
  "btr",
  "btqh",
  "btsc",
  "crs",
  "tpm",
  "cl",
  "clak2",
  "clak3",
  "bs",
  "sshq",
  "shfq",
  "sclq",
  "btv"

              ] -}
  (concat [replicate 4 "clak2", replicate 4 "crs", replicate 4 "tpm", replicate 4 "btv"]) [68..83] [
  (On 90, Action playStack),
  (On 91, Action pushRest),
  (SR 1, Action playSlice),
  (SR 7, Action setDirection),
  (On 92, Action enterBrackets),
  (Off 92, Action leaveBrackets),
  (CC 92, Action pickSample),
  (CC 4, Action setCycleRes),
  (SR 3, Action setTempo)
  ]
