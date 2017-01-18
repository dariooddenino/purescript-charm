module Node.Charm where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn1, runFn1, Fn2, runFn2, Fn3, runFn3)
import Data.Either (Either(..))

-- Types

foreign import data CHARM :: !
foreign import data Charm :: *

type CharmM e a = Eff (charm :: CHARM | e) a
type ChF1 e = Fn1 Charm (CharmM e Unit)
type ChF2 e a = Fn2 Charm a (CharmM e Unit)
type ChF3 e a b = Fn3 Charm a b (CharmM e Unit)

data Region = End | Start | Line | Down | Up | Screen

instance showRegion :: Show Region where
  show End = "end"
  show Start = "start"
  show Line = "line"
  show Down = "down"
  show Up = "up"
  show Screen = "screen"

data Mode = LineMode | CharMode

instance showMode :: Show Mode where
  show LineMode = "line"
  show CharMode = "char"

data Display = Reset | Bright | Dim | Underscore | Blink | Reverse | Hidden

data Colors = Red | Yellow | Green | Blue | Cyan | Magenta | Black | White

instance showColors :: Show Colors where
  show Red = "red"
  show Yellow = "yellow"
  show Green = "green"
  show Blue = "blue"
  show Cyan = "cyan"
  show Magenta = "magenta"
  show Black = "black"
  show White = "white"

type Color = Either Colors Int

getColor :: Color -> String
getColor (Left c) = show c
getColor (Right n) = show n

instance showDisplay :: Show Display where
  show Reset = "reset"
  show Bright = "bright"
  show Dim = "dim"
  show Underscore = "underscore"
  show Blink = "blink"
  show Reverse = "reverse"
  show Hidden = "hidden"

-- Foreign imports

foreign import _init :: Fn1 (Array String) Charm
foreign import _reset :: forall eff. ChF1 eff
foreign import _destroy :: forall eff. ChF1 eff
foreign import _end :: forall eff. ChF1 eff
foreign import _write :: forall eff. ChF2 eff String
foreign import _setPosition :: forall eff. ChF3 eff Int Int
-- foreign import _getPosition ?
foreign import _move :: forall eff. ChF3 eff Int Int
foreign import _up :: forall eff. ChF2 eff Int
foreign import _down :: forall eff. ChF2 eff Int
foreign import _left :: forall eff. ChF2 eff Int
foreign import _right :: forall eff. ChF2 eff Int
-- push
-- pop
foreign import _erase :: forall eff. ChF2 eff String
foreign import _delete :: forall eff. ChF3 eff String Int
foreign import _insert :: forall eff. ChF3 eff String Int
foreign import _display :: forall eff. ChF2 eff String
foreign import _foreground :: forall eff. ChF2 eff String
foreign import _background :: forall eff. ChF2 eff String
foreign import _cursor :: forall eff. ChF2 eff Boolean

-- | Creates a new readable/writable charm stream.
-- | You can pass an array of readable or writable streams.
-- | If the array is empty, the default process is used.
charm :: Array String -> Charm
charm = _init

-- | Resets the entire screen.
reset
  :: forall eff
   . Charm
  -> CharmM eff Unit
reset = runFn1 _reset

-- | Emits an "end" event downstream.
destroy
  :: forall eff
   . Charm
  -> CharmM eff Unit
destroy = runFn1 _destroy

-- | Emits an "end" event downstream.
end
  :: forall eff
   . Charm
  -> CharmM eff Unit
end = runFn1 _end

-- | Passes along a string to the output stream.
write
  :: forall eff
   . Charm
  -> String
  -> CharmM eff Unit
write = runFn2 _write

-- | Sets the cursor position to the absolute coordinates x and y
setPosition
  :: forall eff
   . Charm
  -> Int
  -> Int
  -> CharmM eff Unit
setPosition = runFn3 _setPosition

-- getPosition

-- | Moves the cursor position by the relative coordinates x and y
move
  :: forall eff
   . Charm
  -> Int
  -> Int
  -> CharmM eff Unit
move = runFn3 _move

-- | Moves the cursor up by y rows.
up
  :: forall eff
   . Charm
  -> Int
  -> CharmM eff Unit
up = runFn2 _up

-- | Moves the cursor down by y rows.
down
  :: forall eff
   . Charm
  -> Int
  -> CharmM eff Unit
down = runFn2 _down

-- | Moves the cursor left by x rows.
left
  :: forall eff
   . Charm
  -> Int
  -> CharmM eff Unit
left = runFn2 _left

-- | Moves the cursor right by x rows.
right
  :: forall eff
   . Charm
  -> Int
  -> CharmM eff Unit
right = runFn2 _right

-- push
-- pop

-- | Erases a Region:
-- | End: erases from the cursor to the end of the line.
-- | Start: erases from the cursor to the start of the line.
-- | Line: erases the current line.
-- | Down: erases everything below the current line.
-- | Up: erases everything above the current line.
-- | Screen: erases the entire screen.
erase
  :: forall eff
   . Charm
  -> Region
  -> CharmM eff Unit
erase c = runFn2 _erase c <<< show

-- | Deletes lines or chars. Differs from erase because it does not write over deleted characters with whitespace.
-- | LineMode | CharMode
-- | The cursor position is not updated.
-- | @FIXME: not working?
delete
  :: forall eff
   . Charm
  -> Mode
  -> Int
  -> CharmM eff Unit
delete c m = runFn3 _delete c (show m)

-- | Inserts space into the terminal. insert is the opposite of delete, and the arguments are the same.
insert
  :: forall eff
   . Charm
  -> Mode
  -> Int
  -> CharmM eff Unit
insert c m = runFn3 _insert c (show m)

-- | Sets the display mode to Display
-- | @FIXME: not working?
display
  :: forall eff
   . Charm
  -> Display
  -> CharmM eff Unit
display c = runFn2 _display c <<< show

-- | Sets the foreground color to Either Colors Int
-- | 0 <= Int <= 255
foreground
    :: forall eff
     . Charm
    -> Color
    -> CharmM eff Unit
foreground c = runFn2 _foreground c <<< getColor

-- | Sets the background colro to Either Colors Int
-- | 0 <= Int <= 255
background
    :: forall eff
     . Charm
    -> Color
    -> CharmM eff Unit
background c = runFn2 _background c <<< getColor

-- | Sets the cursor visibility.
cursor
  :: forall eff
   . Charm
  -> Boolean
  -> CharmM eff Unit
cursor = runFn2 _cursor
