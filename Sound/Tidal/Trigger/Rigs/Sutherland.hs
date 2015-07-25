

module Sound.Tidal.Trigger.Rigs.Sutherland (sutherland) where

import Sound.Tidal.Trigger.Types
import Sound.Tidal.Trigger.Actions

pads samples notes = zip notes' samples'
  where
    notes' = map On notes
    samples' = map ((Action).triggerSample) samples


rig samples notes fs = asMapping $ concat [pads samples notes, fs]

sutherland = rig [
  "bd",
  "ac",
  "acs",
  "btr",
  "btqh",
  "btsc",
  "crs",
  "mrbrs",
  "tploud",
  "clak2",
  "clak3",
  "bs",
  "sshq",
  "shfq",
  "sclq",
  "btv"
              ] [68..83] [
  (On 90, Action playStack),
  (On 91, Action pushRest),
  (SR 1, Action playSlice),
  (On 92, Action enterBrackets),
  (Off 92, Action leaveBrackets)
--  (CC 92, Action pickSample)
  ]
