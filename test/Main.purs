module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Node.Charm.Examples

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  lucky
--  log "You should add some tests."
