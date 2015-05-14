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
```

## Usage

Within a running tidal session use one of the following functions

```haskell
runnow d1 $ seqP [(2, 4, sound "bd sn")]
```
or to simply trigger a pattern one cycle

```haskell
oneshot d1 $ sound "bd sn"
```
which is essentially the same as `runnow d1 $ seqP [(0, 1, sound "bd sn")]`

To trigger a pattern for multiple cycles use `oneshot'` and pass in the number of cycles to play

```haskell
oneshot' d1 4 $ sound "bd sn"
```