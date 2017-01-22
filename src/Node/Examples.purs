module Node.Charm.Examples where

import Node.Charm
import Prelude
import Data.Either
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Timer
import Control.Monad.Trans.Class
import Control.Monad.Eff.Class
import Control.Monad.Reader
import Control.Monad.Rec.Class
import Control.Monad.ST
-- import Control.Monad.Aff
import Data.Maybe
import Data.Array
import Data.String (toCharArray)

twofivesix = do
  let c = charm []
      r = render c
  runST do
    r reset
    i <- newSTRef 0
    void $ setInterval 15 do
      step <- readSTRef i
      r do
        background (Right step)
        write " "
      if step == 255
         then writeSTRef i 0
         else writeSTRef i $ step + 1
      pure unit

column :: Eff (charm :: CHARM) Unit
column = do
  let c = charm []
  render c do
    reset
    right 16
    write "beep"
    down 1
    right 32
    write "boop\n"
    end

cursorEx = do
  let c = charm []
  render c do
    reset
    setPosition 5 10
    move 7 2
    push false
    write "lul"
    left 3
    up 1
    foreground $ Left Magenta
    write "v"
    left 1
    up 1
    display Reset
    write "|"
    down 3
    pop false
    background $ Left Blue
    write "popped\npow"
    display Reset
    erase Line
    end

lucky = do
  let c = charm []
      r = render c
      colors = [ Red, Cyan, Yellow, Green, Blue ]
      text = [ "A", "l", "w", "a", "y", "s", " ", "a", "f", "t", "e", "r", " "
             , "m", "e", " ", "l", "u", "c", "k", "y", " ", "c", "h", "a", "r", "m", "s", "."
             ]
      getV :: forall a. Array a -> Int -> Maybe a
      getV arr i = index arr $ i - (length arr) * (i / length arr)
  runST do
    r reset
    offset <- newSTRef 0
    y <- newSTRef 0
    dy <- newSTRef 1
    void $ setInterval 150 do
      writeSTRef y 0
      writeSTRef dy 1
      offsetV <- readSTRef offset
      forE 0 40 \i -> do
        dyV <- readSTRef dy
        yV <- readSTRef y
        let color = getV colors (i + offsetV)
            letter = getV text (i + offsetV)
        r do
          move 1 dyV
          foreground $ Left (fromMaybe Red color)
          write $ fromMaybe "A" letter
        writeSTRef y $ yV + dyV
        writeSTRef dy $ if (yV + dyV <= 0 || yV + dyV >= 5)
                        then -dyV
                        else dyV
        pure unit
      r $ setPosition 0 1
      writeSTRef offset $ offsetV + 1
      pure unit

progress = do
  let c = charm []
      r = render c
  runST do
    r reset
    i <- newSTRef 0
    r $ write "Progress: 0 %"
    void $ setInterval 25 do
      iV <- readSTRef i
      r do
        left ((_ + 2) $ length $ toCharArray $ show iV)
        write (show (iV + 1) <> " %")
      when (iV + 1 >= 100) do
        r end
      writeSTRef i (iV + 1)
      pure unit
    pure unit
