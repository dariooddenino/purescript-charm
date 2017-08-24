module Node.Charm where

import Control.Monad.Trans.Class (lift)
import Prelude
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Reader (ask)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, Fn2, runFn2, Fn3, runFn3)

-- Types

foreign import data CHARM :: Effect
foreign import data Charm :: Type

type CharmEff e a = Eff (charm :: CHARM | e) a
type CharmM e = ReaderT Charm (Eff (charm :: CHARM | e)) Unit

type ChF1 e = Fn1 Charm (CharmEff e Unit)
type ChF2 e a = Fn2 a Charm (CharmEff e Unit)
type ChF3 e a b = Fn3 a b Charm (CharmEff e Unit)

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
foreign import _getPosition :: forall eff. Fn2 (Int -> CharmEff eff Unit) Charm (CharmEff eff Unit)
foreign import _move :: forall eff. ChF3 eff Int Int
foreign import _up :: forall eff. ChF2 eff Int
foreign import _down :: forall eff. ChF2 eff Int
foreign import _left :: forall eff. ChF2 eff Int
foreign import _right :: forall eff. ChF2 eff Int
foreign import _push :: forall eff. ChF2 eff Boolean
foreign import _pop :: forall eff. ChF2 eff Boolean
foreign import _erase :: forall eff. ChF2 eff String
foreign import _delete :: forall eff. ChF3 eff String Int
foreign import _insert :: forall eff. ChF3 eff String Int
foreign import _display :: forall eff. ChF2 eff String
foreign import _foreground :: forall eff. ChF2 eff String
foreign import _background :: forall eff. ChF2 eff String
foreign import _cursor :: forall eff. ChF2 eff Boolean

-- Functions


render :: forall eff. Charm -> CharmM eff -> CharmEff eff Unit
render = flip runReaderT


-- | Creates a new readable/writable charm stream.
-- | You can pass an array of readable or writable streams.
-- | If the array is empty, the default process is used.
charm :: Array String -> Charm
charm = _init


-- | Resets the entire screen.
-- reset
--   :: forall eff
--    . Charm
--   -> CharmEff eff Unit

reset :: forall eff. CharmM eff
reset = ask >>= lift <<< _reset


-- -- | Emits an "end" event downstream.
destroy :: forall eff. CharmM eff
destroy = ask >>= lift <<< _destroy


-- -- | Emits an "end" event downstream.
end :: forall eff. CharmM eff
end = ask >>= lift <<< _end


-- -- | Passes along a string to the output stream.
write :: forall eff. String -> CharmM eff
write s = ask >>= lift <<< runFn2 _write s


-- -- | Sets the cursor position to the absolute coordinates x and y
setPosition :: forall eff. Int -> Int -> CharmM eff
setPosition x y = ask >>= lift <<< runFn3 _setPosition x y


-- getPosition' :: forall eff. Charm -> Aff (charm :: CHARM | eff) Int
-- getPosition' c = makeAff (\_ success -> runFn2 _getPosition success c)

-- getPosition :: forall eff. ReaderT Charm (Eff (charm :: CHARM, err :: EXCEPTION | eff)) Unit
-- getPosition = do
--   c <- ask
--   lift $ void $ launchAff $ getPosition' c


-- -- | Moves the cursor position by the relative coordinates x and y
move :: forall eff. Int -> Int -> CharmM eff
move x y = ask >>= lift <<< runFn3 _move x y


-- -- | Moves the cursor up by y rows.
up :: forall eff. Int -> CharmM eff
up y = ask >>= lift <<< runFn2 _up y

-- -- | Moves the cursor down by y rows.
down :: forall eff. Int -> CharmM eff
down y = ask >>= lift <<< runFn2 _down y

-- -- | Moves the cursor left by x rows.
left :: forall eff. Int -> CharmM eff
left x = ask >>= lift <<< runFn2 _left x

-- -- | Moves the cursor right by x rows.
right :: forall eff. Int -> CharmM eff
right x = ask >>= lift <<< runFn2 _right x

-- | Pushes the cursor state and optionally the attribute state.
push :: forall eff. Boolean -> CharmM eff
push w = ask >>= lift <<< runFn2 _push w

-- | Pops the cursor state and optionally the attribute state.
pop :: forall eff. Boolean -> CharmM eff
pop w = ask >>= lift <<< runFn2 _pop w

-- -- | Erases a Region:
-- -- | End: erases from the cursor to the end of the line.
-- -- | Start: erases from the cursor to the start of the line.
-- -- | Line: erases the current line.
-- -- | Down: erases everything below the current line.
-- -- | Up: erases everything above the current line.
-- -- | Screen: erases the entire screen.
erase :: forall eff. Region -> CharmM eff
erase r = ask >>= lift <<< runFn2 _erase (show r)

-- -- | Deletes lines or chars. Differs from erase because it does not write over deleted characters with whitespace.
-- -- | LineMode | CharMode
-- -- | The cursor position is not updated.
-- -- | @FIXME: not working?
delete :: forall eff. Mode -> Int -> CharmM eff
delete m n = ask >>= lift <<< runFn3 _delete (show m) n


-- -- | Inserts space into the terminal. insert is the opposite of delete, and the arguments are the same.
insert :: forall eff. Mode -> Int -> CharmM eff
insert m n = ask >>= lift <<< runFn3 _insert (show m) n


-- -- | Sets the display mode to Display
-- -- | @FIXME: not working?
display :: forall eff. Display -> CharmM eff
display d = ask >>= lift <<< runFn2 _display (show d)


-- -- | Sets the foreground color to Either Colors Int
-- -- | 0 <= Int <= 255
foreground :: forall eff. Color -> CharmM eff
foreground c = ask >>= lift <<< runFn2 _foreground (getColor c)


-- -- | Sets the background colro to Either Colors Int
-- -- | 0 <= Int <= 255
background :: forall eff. Color -> CharmM eff
background c = ask >>= lift <<< runFn2 _background (getColor c)


-- -- | Sets the cursor visibility.
cursor :: forall eff. Boolean -> CharmM eff
cursor v = ask >>= lift <<< runFn2 _cursor v
