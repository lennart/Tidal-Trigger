module Sound.Tidal.Trigger.Context (module C, tstate, trigger) where

import qualified Data.Map.Strict as Map
import Control.Monad
import Data.Maybe
import Control.Concurrent

import Sound.Tidal.Trigger.Stream as C
import Sound.Tidal.Trigger.Types as C
import Sound.Tidal.Trigger.Actions as C
import qualified Sound.Tidal.Context as T

tstate = trigState

{-
ultimatively I want the following to be a generic mapping:

MIDI Note On Key 65 Velocity 90 -> Pattern OscMap

or simply: a -> b

now the "Context" keeps a list of concrete mappings in the style of:

MIDI Note On Key 65 -> Int -> Pattern OscMap

Which is what I would call an "Action", It takes a prefix notation (Path) of an event and a value (matching the type of the input event) and produces a output value, in this case a Pattern of an OscMap

To better reflect the artistic part of all this, I call the Context a "Performance"

Initially the performance is setup with a state of the output, which is in most cases plain emptiness. In case of Dirt this is silence.

The list of performable Actions is empty, as no input events have been received and no output sockets are available.

As soon as Input Events come in, their execution is postponed until a matching output socket is found and a mapping for input prefix and a output socket is stored within the performance

As soon as Output State is modified, output sockets are created for each part of the output, in the case of a Pattern OscMap, for each OscMap Pair in the pattern, e.g. a Param and a Datum

Sockets can be composed to an action with a found mapping between a Path, a concrete input value and the type of datum that is necessary for the param of the OscMap Pair.

Generalisation:

To make playing this instrument useful, certain generalizations should be possible:

Since e.g. MIDI only will supply integer values and some of dirt's params only take e.g. String or Double values, randomly picking from Transformers between Int and Double|String to make working Actions is necessary, otherwise most of Dirt's functionality won't be controllable via MIDI.

The other way around, when using OSC as an input and MIDI as an output, would require Transformers for converting Double|String to Int and randomly pick them to make mappings work.

TODO:

- implement transformers

- implement an adaptor for transformers to feed in markov chains, allow consumed programmed values to be training the chain and use given input to produce new values from the chain in the _Style_ of the trained data, e.g. when feeding in given sample names, integer values received and mapped to the given param will choose from the values entered via programming with a probability taken from the chain

-}


-- return a pattern that contains only the given param
matchingParam :: (Eq k, Functor m) => k -> KeyMap m k v -> KeyMap m k v
matchingParam prm p = fmap (\x -> Map.filterWithKey (\k _ -> k == prm) x) p

-- take a param and a function that maps an input to a pattern
-- use input and a pattern to apply the input to the given param to produce a new pattern with only the given param in each event
runParam :: (Eq k, Functor m) => Input -> KeyMap m k v -> (k, Input -> KeyMap m k v) -> KeyMap m k v
runParam i p (prm, f) = matchingParam prm $ f i


toActionPaths :: ActionMap m k v -> [Path]
toActionPaths a = map fst $ Map.keys a

toActionParams :: ActionMap m k v -> [k]
toActionParams a = map snd $ Map.keys a

toActionMap :: (Ord k) => [(k, Input -> KeyMap m k v)] -> Path -> ActionMap m k v
toActionMap values p = Map.fromList $ zipWith f keys values
  where
    keys = replicate (length values) p
    f a b = ((a, fst b), b)

findAction :: TriggerEvent -> Performance m k v -> Maybe ((k, Input -> KeyMap m k v))
findAction e perf
  | (Map.size as') == 0 = Nothing
  | otherwise = Just $ snd $ Map.elemAt 0 as'
  where
    as' = Map.filterWithKey (\k _ -> (fst k) == p) as
    as = actions perf
    p = tpath e

findFreeParam :: (Eq k) => KeyMap m k v -> Performance m k v -> TriggerEvent -> Maybe (k, Input -> KeyMap m k v)
findFreeParam a perf e
  | (length free) == 0 = Nothing
  | otherwise = Just $ head free
  where
    i = tinput e
    p = tpath e
    r = rig perf
    as = actions perf
    w = welder r i a
    free = filter (\k -> notElem (fst k) $ toActionParams as) w

loopSet :: MVar (a, [a]) -> a -> [a] -> IO ()
loopSet s a as = do
  res <- tryPutMVar s (a, as)
  case res of
    True -> do
      return ()
    False -> do
      threadDelay 1000
      loopSet s a as

-- this might:
-- 1a. add an action to the list of mapped actions for a free param and the input event signature -- does not work, no new actions are added
-- 1b. reuse an existing action that is mapped to a given param for the input event siganture -- fail
-- 2a. perform the action
-- 2b. do nothing if no free param can be mapped for this input event signature
handleKey :: (Show k, Ord k, Eq k, Functor m) => Performance m k v -> TriggerEvent -> IO (Performance m k v)
handleKey perf e = do
  let pth = tpath e
      i = tinput e
      s = state $ rig perf
      r = rig perf
      w = welder r
  pps <- tryTakeMVar s
  case pps of
    Just (p, ps) -> do
      let wmap = w i
          action = findAction e perf
          newAction = findFreeParam p perf e
          action' = case action of { Nothing -> newAction; Just a' -> Just a' }
          paramActions = case action' of { Nothing -> Map.fromList []; Just a -> toActionMap [a] pth } -- actions of free params
          performed = case action' of { Nothing -> []; Just a -> map (runParam i p) [a] } -- make patterns from value functions and event input for free params
          newactions = Map.union actions' paramActions -- merged actions
          mrg = merge r
          actions' = actions perf
          performed' = foldl mrg p performed -- merge all performed patterns with the current one (overwriting existing params)
      forkIO $ loopSet s performed' ps
      case (newAction, action) of
        (Nothing, Nothing) -> return perf -- do nothing
        (Just _, Just _) -> return perf -- given action performed, no change
        (Just a, Nothing) -> do -- new action
          putStrLn ("new action: " ++ (show $ fst a) ++ " for event: " ++ (show $ i))
          return perf { actions = newactions }
        (Nothing, Just _) -> return perf -- given action performed, no change
    Nothing -> do
      return perf


trigger latency (d, m, w, t) inputReaders = do
  readers <- sequence inputReaders
  trigger' latency (Rig d m w t) readers

trigger' latency rig readers = do
  (cps, getNow) <- T.cpsUtils
  eventsM <- newMVar []
  let perf = Perf { actions = (asMapping []), rig = rig, tempo = 1.0, sockets = [], pending = [] }
  forkIO $ loop perf readers eventsM
  forkIO $ eventLoop readers eventsM
  return perf
    where loop perf readers eventsM = do
            perf' <- act perf eventsM
            threadDelay latency
            loop perf' readers eventsM
          eventLoop readers eventsM = do
            modifyMVar_ eventsM $ \events -> do
              events' <- liftM concat $ sequence readers
              return $ events ++ events'
            eventLoop readers eventsM
          act perf eventsM = do
            events <- readMVar eventsM
            foldM handleKey perf events
