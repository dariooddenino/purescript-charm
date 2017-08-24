module Node.Charm.Examples where

import Node.Charm
import Prelude

import Control.Monad.Aff (Aff, delay, launchAff)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.ST (ST, modifySTRef, newSTRef, readSTRef, writeSTRef)
import Data.Array (index, length)
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe)
import Data.String (toCharArray)
import Data.Time.Duration (Milliseconds(..))

sleep :: forall e. Aff e Unit
sleep = delay (Milliseconds 30.0)

-- twofivesix :: forall e s. Eff (charm :: CHARM, st :: ST s, exception :: EXCEPTION | e) Unit
twofivesix = void $ launchAff $ do
  let c = charm []
      r = render c
  n <- liftEff do
    r reset
    newSTRef 0
  innerLoop r n
  where
    innerLoop r n = do
      step <- liftEff $ readSTRef n
      liftEff $ r do
        _ <- background (Right step)
        write " "
      _ <- if step == 255
           then do
             liftEff $ r end
             pure unit
           else do
             _ <- sleep
             _ <- liftEff $ modifySTRef n (_ + 1)
             innerLoop r n
      pure unit


twofivesix' :: forall e. Eff (charm :: CHARM | e) Unit
twofivesix' = do
  let c = charm []
      r = render c
  r reset
  forE 0 255 \i -> do
    r do
      background (Right i)
      write " "
    pure unit
  r $ display Reset
  r end
  pure unit

column :: forall e. Eff (charm :: CHARM | e) Unit
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

cursorEx :: forall e. Eff (charm :: CHARM | e) Unit
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

getV :: forall a. Array a -> Int -> Maybe a
getV arr i = index arr $ i - (length arr) * (i / length arr)

lucky :: forall e s. Eff (charm :: CHARM, st :: ST s, exception :: EXCEPTION | e) Unit
lucky = void $ launchAff do
  liftEff $ r reset
  offset <- liftEff $ newSTRef 0
  y <- liftEff $ newSTRef 0
  dy <- liftEff $ newSTRef 1
  innerLoop r colors text offset y dy
  where
    c = charm []
    r = render c
    colors = [ Red, Cyan, Yellow, Green, Blue ]
    text = [ "A", "l", "w", "a", "y", "s", " ", "a", "f", "t", "e", "r", " "
           , "m", "e", " ", "l", "u", "c", "k", "y", " ", "c", "h", "a", "r", "m", "s", "."
           ]
    innerLoop r colors text offset y dy = do
      _ <- sleep
      _ <- liftEff do
        writeSTRef y 0 *>
        writeSTRef dy 1
      offsetV <- liftEff $ readSTRef offset
      liftEff $ forE 0 40 \i -> do
        dyV <- readSTRef dy
        yV <- readSTRef y
        let
          color = getV colors (i + offsetV)
          letter = getV text (i + offsetV)
        r do
          move 1 dyV
          foreground $ Left (fromMaybe Red color)
          write $ fromMaybe "A" $ letter
        _ <- writeSTRef y $ yV + dyV
        _ <- writeSTRef dy $ if (yV + dyV <= 0 || yV + dyV >= 5)
                             then -dyV
                             else dyV
        pure unit
      _ <- liftEff do
        r $ setPosition 0 1
        writeSTRef offset $ offsetV + 1
      if offsetV > 300
        then (liftEff $ r end) *> pure unit
        else innerLoop r colors text offset y dy

progress :: forall e s. Eff (charm :: CHARM, st :: ST s, exception :: EXCEPTION | e) Unit
progress = void $ launchAff do
  let c = charm []
      r = render c
  liftEff $ r do
    reset
    write "Progress: 0 %"
  i <- liftEff $ newSTRef 0
  innerLoop r i
  where
    innerLoop r i = do
      i' <- liftEff $ readSTRef i
      liftEff $ r do
        left ((_ + 2) $ length $ toCharArray $ show i')
        write (show (i' + 1) <> " %")
      if (i' + 1 >= 100)
        then do
          liftEff $ r end
          pure unit
        else do
          _ <- sleep
          _ <- liftEff $ modifySTRef i (_ + 1)
          innerLoop r i
