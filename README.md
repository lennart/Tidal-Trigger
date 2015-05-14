# Tidal Trigger

based upon the live coding pattern language [tidal](http://tidal.lurk.org) this module allows triggering tidal patterns directly without looping them.

Tidal Trigger was created to more easily use tidal in an improvising context together with acoustic instruments, leveraging tidals expressiveness via patterns and haskell's functional programming style while being able to instantly react to other musicians not bound to any concept of _tempo_ or a _clock_.

It should fit in between cycle oriented live coding using _vanilla_ tidal and its counterpart for compositions (i.e. sequencing patterns via `seqP`).

Technically speaking tidal trigger will evaluate one cycle of a pattern and directly send it to the underlying stream via osc. This allows using tidal's standard sampling backend [Dirt](http://github.com/tidalcycles/Dirt) and also talk to MIDI devices via [tidal-midi](http://github.com/tidalcycles/tidal-midi).

## Install

```shell
git clone https://github.com/lennart/Tidal-Trigger
cd Tidal-Trigger
cabal install
```

## Setup (for emacs)

To make use of Tidal Trigger in emacs, add the following to your `tidal.el` within `(tidal-start-haskell ...`

```emacs
(tidal-send-string "import Sound.Tidal.Trigger")

(tidal-send-string "tr1 <- makeTrigger dirt 7771")
```

## Usage

Within a running tidal session use `tr1` just like you would use e.g. a dirt stream `d1`:

```haskell
tr1 $ sound "bd sn"
```

This will run the above pattern for exactly one cycle

Note that you could reproduce this with _vanilla_ tidal via:

```haskell
d1 $ seqP [(0, 1, sound "bd sn")]
```

In emacs you can use `retrig` to make this work any time you like:

```haskell
d1 $ retrig $ seqP [(0, 1, sound "bd sn")]
```

## Bugs

Currently this will at some point open too many files and break tidal's server with `*** Exception: getCurrentDirectory: resource exhausted (Too many open files)`.
Investigation on why and how to fix it is already started…

Workaround is to restart the tidal interpreter (`C-c C-q` then `C-c C-s` in emacs)