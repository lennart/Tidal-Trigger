-- Tidal Trigger

-- > {-# LANGUAGE FlexibleInstances #-}

> module Sound.Tidal.Trigger.Context where

-- >

> import Sound.OSC.FD

-- > import Control.Concurrent
-- > import Data.Maybe
-- > import qualified Sound.PortMidi as PM
-- > import Sound.Tidal.Trigger
-- > import Data.Char
-- > import Sound.Tidal.Trigger.Device
-- > import qualified Data.ByteString.Char8 as B
-- > import Sound.Tidal.Dirt
-- > import Sound.Tidal.SimpleSynth
-- >


-- a trigger is defined by a kind

-- > --data Trigger = NoOp | MIDI.Note | OSC String
-- >


-- input event type class



-- > class InputPacket a where
-- >   signature :: a -> InputValue
-- >   inI :: a -> Int
-- >   inF :: a -> Float
-- >   inS :: a -> String



-- implementation for osc and midi inputs

-- > instance InputPacket Message where
-- >   signature (Message path _) = Osc path
-- >   inI (Message _ args) = (fromJust $ d_get $ args !! 0) :: Int
-- >   inF (Message _ args) = (fromJust $ d_get $ args !! 0) :: Float
-- >   inS (Message _ args) = (B.unpack $ fromJust $ d_get $ args !! 0) :: String
-- >
-- > instance InputPacket PM.PMEvent where
-- >   signature (PM.PMEvent { PM.message = PM.PMMsg { PM.status = 0xB0, PM.data1 = c } }) = CC $ fromIntegral c
-- >   signature (PM.PMEvent { PM.message = PM.PMMsg { PM.status = 0x90, PM.data1 = k } }) = Note $ fromIntegral k
-- >   inI e = fromIntegral $ PM.data2 $ PM.message e
-- >   inF e = (/) 127 $ fromIntegral $ inI e
-- >   inS e = replicate 1 $ chr $ inI e

-- an input contains a name, a protocol, some contain a port

-- > data InputDevice a = { readDev :: InputPacket b => a -> IO [b] }



-- data type for

-- an output contains a name and a protocol

-- > data Output = MidiOut String | OscOut String Int
-- >

-- connects a trigger an action an input and and output

-- > --connect t a i o = t i on'
-- > --  where
-- > --    on' e = send o a e


-- sends a message through output generates params with action `a`, expects input to accept a handler function that gets invoked with the latest value as its sole arguments

-- > --send o action arg =
-- > --  where
-- > --    arg' = action arg

-- trigger an action one the specifying type is recorded

-- > --trig kind reader input handler = handlers'
-- >   --where
-- >   --  handlers' =
-- > --bind = trig
-- >

-- add a midi input listener

-- > midiin name = midiIn name

-- > midiout name = midiOut name

-- add a osc input listener

-- > oscin host port = Just $ udpServer host port

-- > oscout host port = do
-- >                      x <- openUDP host port
-- >                      return $ Just x

-- take control of a device

-- > handleDevice dev handler = do
-- >   forkIO $ loop dev
-- >   return ()
-- >     where loop dev = do
-- >             e <- read dev
-- >             mapM_ handler e
-- >             threadDelay 1000
-- >             loop dev

-- read an event from an input

-- > -- instance InputDevice UDP where
-- > --          readDev dev = do
-- > --                        r <- recvMessage dev
-- > --                        case r of
-- > --                          Nothing -> return []
-- > --                          Just m -> return [m]
-- >
-- >
-- >
-- > --instance InputDevice PM.PMStream where
-- > --         readDev dev = do
-- > --                       e <- PM.readEvents dev
-- > --                       case e of
-- > --                         Right PM.NoError -> return []
-- > --                         Left x -> return x
-- >
-- >

-- handle Incoming Events



-- synopsis

-- make a new mapping

-- > data InputValue = CC Int | Note Int | Osc String deriving (Show, Eq)
-- >
-- > data Action a b c d = A InputValue a (b -> c) d


-- > cc x = A (CC x)
-- > osc x = A (Osc x)
-- > n x = A (Note x)

-- > -- transform f l =

-- > applyTransform e (A a i t c) = t e
-- >
-- >
-- > isTriggered e (A a x t c) = (signature e) == x
-- >
-- > bridge Nothing Nothing as = do
-- >   putStrLn ("Only Dummy devices found")
-- > bridge Nothing (Just o) as = do
-- >   putStrLn ("Input Dummy device found")
-- > bridge (Just i) Nothing as = do
-- >   putStrLn ("Output Dummy debice found")
-- > bridge (Just i) (Just o) as = do
-- >   putStrLn ("Fine, good to go with input output")
-- > -- bridge i o as =
-- > --   handleDevice i $ \e -> do
-- > --   let as' = filter (isTriggered e) as
-- > --        as'' = map (applyTransform e) as'
-- > --    return fmap
-- >

-- define example mapping

-- > --led x = x

-- > --neotetra i o = bridge i o $
-- > --               concat [
-- > --                 [A (CC 64) ((/127).fromIntegral.inI) speed]]
-- > --                  A (CC 90) (((-) 1).(*2).(/127).inI) now],
-- > --                  map (note.inI) (map (\x -> A x ) (map (Note) [40..90]))]
-- >
-- > --neotetra' = do
-- > --      quneo <- midiin "QUNEO"
-- > --      dirt <- oscout "127.0.0.1" 7771
-- > --      let neo = neotetra quneo dirt
-- > --      return neo
-- >
-- >
-- > --dirtqu = bridge (oscin "127.0.0.1" 6010) (midiout "QUNEO") [
-- > --  A osc "/recording/heartbeat" (*127).(/100).inF led
-- > --                                        ]


-- simple note passthru

-- _OPTIMIZE: make this a hardware mapping as well, so this connection will be able to work even if this haskell program is not, even with complex logic)_

-- > -- t1 = trig MIDI.Note.On.Key
-- > -- i1 = midiin "QUNEO"
-- > -- o1 = midiout "DSI Tetra"
-- > -- c1 = connect t1 note i o

-- map recording heartbeat received from dirt, appear as moving led circle on QUNEO

-- > -- t2 = trig $ OSC "/recording/heartbeat"
-- > -- i2 = oscin "127.0.0.1" 7771
-- > -- o2 = midiout "QUNEO"
-- > -- c2 = connect t2 led i o

-- map the midi control change no. 64 (find a better way to name this), to the current speed of dirt's sample playback, e.g. pressure of a pad

-- > -- t3 = trig MIDI.CC 64
-- > -- i3 = midiin "QUNEO"
-- > -- o3 = oscout "127.0.0.1" 7771
-- > -- c3 = connect t3 speed i o

-- map cc90 to advance time on any event by one step

-- > -- t4 = trig MIDI.CC 90
-- > -- i4 = midiin "QUNEO"
-- > -- o4 = trigout "now"
-- > -- c4 = connect t4 (\x -> 1 - (2 * x / 127)) i o -- an inline range mapping from -1 to 1

-- the above uses the tidal-trigger output with the param "now" to control playhead. this can be used as a scratch control, or mapped to an automaton producing constant timed events for "regular" playback.

-- use list of and replicate
