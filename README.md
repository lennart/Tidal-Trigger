# Tidal Trigger

based upon the live coding pattern language [tidal](http://tidal.lurk.org) this module allows triggering tidal patterns directly without looping them.

Tidal Trigger was created to more easily use tidal in an improvising context together with acoustic instruments, leveraging tidals expressiveness via patterns and haskell's functional programming style while being able to instantly react to other musicians not bound to any concept of _tempo_ or a _clock_.

This version allows defining filters for input actions on certain devices to trigger standard functions to interact with a "stack" of samples.

You can push samples to a stack, play the whole stack, play only a fraction of it at a certain point in time, pop the whole stack leaving it empty.

Additionally this adds Serial input and allows registering e.g. Events read from a rotary encoder connected to an arduino.

## Install

```shell
git clone --branch actions https://github.com/lennart/Tidal-Trigger
cd Tidal-Trigger
cabal install
```

## Setup (for emacs)

To make use of Tidal Trigger in emacs, add the following to your `tidal.el` within `(tidal-start-haskell ...`

```emacs
(tidal-send-string "import Sound.Tidal.Trigger")
```
and after loading `cpsUtils`

```emacs
  (tidal-send-string "stream' <- openUDP \"127.0.0.1\" 7771")
  (tidal-send-string "let stream'' = (dirt, stream')")
  (tidal-send-string "trigger <- sampleproxy 1000 \"QUNEO\" \"/dev/tty.usbserial\" stream''")
````
## Usage

Currently mappings are hardwired, Notes 90 pops the stack playing back all stored samples, notes 68..83 add samples to the stack, 91 adds a rest to the stack and an arduino connected via usb sends rotary encoder values to be used as a jogwheel to play a certain part of the _stack_

## TODO

- Make mapping arbitrary
- Allow dirt's params to be controlled via midi/arduino
- make two stacks
- allow popping single samples from the stack
- allow reversing the stack
- 
