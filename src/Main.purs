module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Node.Charm

main :: forall e. Eff (console :: CONSOLE, charm :: CHARM | e) Unit
main = do
  log "Hello sailor!"
  let c = init
  reset c
  background c "black"
  foreground c "green"
  move c 0 0
  write c "#######"
  move c (-6) 1
  foreground c "white"
  write c "@@@@@@@@"
