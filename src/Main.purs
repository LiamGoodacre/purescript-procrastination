module Main where

import Prelude (Unit, unit, discard, (-))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Data.List (range)
import Procrastination (StrictList(..), DeferList(..), findMap, class Defer)

bananas :: (Defer => Maybe Unit) -> (Defer => Maybe Unit) -> Unit
bananas Nothing r = unit
bananas l r = unit

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  -- a somewhat big list
  let list = range 1 10000

  -- expensive computation for checking an element in the list
  let pred x =
        let bigComputation 0 = Just x
            bigComputation i = bigComputation (i - 1)
        in bigComputation 10000
  log "Loaded"

  log "Starting finding with strict append"
  let foundStrict = findMap pred (SL list)
  log "Done"

  log "Starting finding with defer append"
  let foundDefer = findMap pred (DL list)
  log "Done"

