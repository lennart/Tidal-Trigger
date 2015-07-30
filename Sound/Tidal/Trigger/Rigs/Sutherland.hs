module Sound.Tidal.Trigger.Rigs.Sutherland (sutherland) where

import Data.Ratio
import Sound.Tidal.Trigger.Types
import Sound.Tidal.Trigger.Actions

import Sound.Tidal.Context as T

-- pads samples notes = zip notes' samples'
--   where
--     notes' = map On notes
--     samples' = map (\s -> ((Action).(\e -> (T.sound s)))) samples

-- c x = T.run 16

--rigmap f a b =

--data Rig = Rig

--fmap (\x y -> (On x, (Action).triggerSample y)) ((+68) <$> (c 4 4)) "clak2/crs"

r = replicate


--rig :: Rig Material
--rig (s,d,r) = Rig ()

--rig samples  = asMapping $ concat [pads samples notes, fs]
sutherland = 2
--sutherland = rig  [
{-
you do not want to know or edit midi cc numbers, osc paths or serial protocols

feed a rig with:

- a list of words: read as sample names
- a list digits: read as integer values for input to time / counter params or actions
- a list of ratios: read as ratio, usages: as both input values for euklid algorithm e.g. 5 8, as input for sample length in striate' e.g. 1/100

returns



optional: this can also be supplied as a string in random order like:

"bd 2 1/2 snarls a e"

MAP them accordingly to their type signature to matching hole, i.e. a matching functor?!




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

              ]
  (concat [r 4 "clak2", r 4 "crs", r 4 "tpm", r 4 "btv"]) [68..83] [
  (On 90, Action playStack),
  (On 91, Action pushRest),
  (SR 1, Action playSlice),
  (SR 7, Action setDirection),
  (On 92, Action enterBrackets),
  (Off 92, Action leaveBrackets),
  (CC 92, Action pickSample),
  (CC 4, Action setCycleRes),
  (SR 3, Action setTempo)
  ]-}
