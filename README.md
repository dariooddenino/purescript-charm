Purescript Charm
================

A PureScript wrapper for [Charm](https://github.com/substack/node-charm)

Example
=======

```purescript
import Prelude
import Node.Charm

main :: Eff (charm :: CHARM) Unit
main = do
    let c = charm []
    render c do
        reset
        right 16
        write "beep"
        down 1
        right 32
        foreground $ Left Magenta
        write "boop\n"
        display Reset
        end
```

Missing
=======

- Right now `purescript-charm` only works with the default `process.stdin` / `process.stdout`.

- `getPosition` doesn't work.
