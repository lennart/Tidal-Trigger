module Sound.Tidal.Trigger.Rigs.Sutherland (sutherland, weld) where

import Data.Ratio
import Data.Maybe
import qualified Data.Map.Strict as Map
import Sound.Tidal.Trigger.Types
import Sound.Tidal.Trigger.Actions

import qualified Sound.OSC.FD as O

import qualified Sound.Tidal.Context as T

-- pads samples notes = zip notes' samples'
--   where
--     notes' = map On notes
--     samples' = map (\s -> ((Action).(\e -> (T.sound s)))) samples

-- c x = T.run 16

--rigmap f a b =

--data Rig = Rig

--fmap (\x y -> (On x, (Action).triggerSample y)) ((+68) <$> (c 4 4)) "clak2/crs"


--rig :: Rig Material
--rig (s,d,r) = Rig ()

--rig samples  = asMapping $ concat [pads samples notes, fs]
sutherland = 2


-- list of params, the given input will work on
weld :: Input -> T.OscPattern -> [(T.Param, Input -> T.OscPattern)]
weld i p = makeActions oscmap i
  where
     oscmap = map snd $ T.seqToRelOnsets (0, 1) p

-- currently only acts upon the first oscmap in a pattern, which seems to be enough for finding out matching actions
makeActions :: [T.OscMap] -> Input -> [(T.Param, Input -> T.OscPattern)]
makeActions [] i = []
makeActions (o:[]) i = catMaybes $ zipWith weld' keys is
  where
    is = replicate (length keys) i
    keys = Map.keys o
makeActions (o:os) i = makeActions [o] i

weld' :: T.Param -> Input -> Maybe (T.Param, Input -> T.OscPattern)
weld' p@(T.I n d) (IVInt i) = Just $ (p, f p)
  where
    f prm = (\x -> (T.listToPat $ replicate 1 $ Map.singleton prm $ mval' x)) :: Input -> T.OscPattern
    mval' x = fmap (O.int32 . fromIntegral) $ mval x
    mval x = (i_get x :: Maybe Int)
weld' p@(T.F n d) (IVDouble i) = Just $ (p, \x ->  (T.listToPat $ replicate 1 $ Map.singleton p $ fmap (O.float . realToFrac) (i_get x :: Maybe Double)))
weld' p@(T.S n d) (IVString i) = Just $ (p, \x -> (T.listToPat $ replicate 1 $ Map.singleton p $ fmap (O.string) (i_get x :: Maybe String)))
weld' x y = Nothing
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
