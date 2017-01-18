module Main where

import Prelude
import Node.Charm
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))

main :: forall e. Eff (console :: CONSOLE, charm :: CHARM | e) Unit
main = do
  let c = charm []
  reset c
  background c (Left Green)
  erase c End
  foreground c (Right 200)
  write c "~~~~"
  setPosition c 0 4
  write c "~~~~"
  left c 1
  up c 1
  write c "#"
  move c (-1) (-1)
  write c "#"
  setPosition c 0 2
  background c (Left Yellow)
  insert c LineMode 30
  display c Bright
  -- foreground c "white"
  --write c "@@@@@@@@"
  -- setPosition c (1) 4
  -- write c "!!!!!"
