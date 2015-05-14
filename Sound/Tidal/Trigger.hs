module Sound.Tidal.Trigger where

import Sound.Tidal.Stream
import Sound.Tidal.Tempo
import Sound.Tidal.Pattern

import Sound.OSC.FD

import Data.Maybe
import Data.Ratio

import Control.Concurrent.MVar
import Control.Exception as E


makeTrigger shape port = do s <- openUDP "127.0.0.1" port
                            return (\p' -> trigger s shape p')

trigger s shape p' = do tempoM <- tempoMVar
                        tempo <- readMVar tempoM
                        let messages = concat $ map (triggerTick s shape tempo p') [0..(ticksPerCycle-1)]
                        E.catch (sequence_ messages) (\msg -> putStrLn $ "oops " ++ show (msg  :: E.SomeException))                
                        return () 

triggerTick s shape t p' tick = mapMaybe toMessage' onsets
  where
    tick' = (fromIntegral tick) :: Integer
    a = tick' % ticksPerCycle
    b = (tick' + 1) % ticksPerCycle
    toMessage' = toMessage s shape t tick
    onsets = seqToRelOnsets (a, b) p'
