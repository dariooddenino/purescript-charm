module Node.Charm where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried 
    (Fn1, runFn1, Fn2, runFn2, Fn3, runFn3)

foreign import data CHARM :: !
foreign import data Charm :: *

foreign import _init :: Charm

type CharmM e a = Eff (charm :: CHARM | e) a

init :: Charm
init = _init

foreign import _reset
    :: forall eff
     . Fn1 Charm
       (CharmM eff Unit)

reset
    :: forall eff
     . Charm
    -> CharmM eff Unit
reset = runFn1 _reset

foreign import _move
    :: forall eff
     . Fn3 Charm
       Int
       Int
       (CharmM eff Unit)

move
    :: forall eff
     . Charm
    -> Int
    -> Int
    -> CharmM eff Unit
move = runFn3 _move

foreign import _background
    :: forall eff
     . Fn2 Charm
       String
       (CharmM eff Unit)

background
    :: forall eff
     . Charm
    -> String
    -> CharmM eff Unit
background = runFn2 _background

foreign import _foreground
    :: forall eff
     . Fn2 Charm
       String
       (CharmM eff Unit)

foreground
    :: forall eff
     . Charm
    -> String
    -> CharmM eff Unit
foreground = runFn2 _foreground

foreign import _write
    :: forall eff
     . Fn2 Charm
       String
       (CharmM eff Unit)

write
    :: forall eff
     . Charm
    -> String
    -> CharmM eff Unit
write = runFn2 _write