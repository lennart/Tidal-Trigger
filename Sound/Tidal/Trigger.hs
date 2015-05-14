module Sound.Tidal.Trigger where

import Sound.Tidal.Context

runnow d p' = do (cps, getNow) <- bpsUtils
                 now <- getNow
                 d $ (now + 0.1) ~> p'


oneshot d p' = runnow d $ seqP [(0, 1, p')]

oneshot' d n p' = runnow d $ seqP [(0, n, p')]

