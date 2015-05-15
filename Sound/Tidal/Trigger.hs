module Sound.Tidal.Trigger where

import Sound.Tidal.Context

runnow g d p' = do now <- g
                   d $ (now + 0.1) ~> p'

oneshot g d p' = runnow g d $ seqP [(0, 1, p')]

oneshot' g d n p' = runnow g d $ seqP [(0, n, p')]

