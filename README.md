# redo

An implementation of djb's [redo](http://cr.yp.to/redo.html) in [Haskell](https://www.haskell.org/). What is redo? Redo provides you a way to rebuild source files when they have changed.

## Installation

To install redo, first make sure you have [GHC](https://www.haskell.org/ghc/) installed. For some, it may be easier to just install the whole [Haskell Platform](https://www.haskell.org/platform/).

Next, clone this repository and run:

    ./do

in the top level directory. A `bin/` directory will be created with the `redo`, `redo-ifchange`, `redo-ifcreate`, and `redo-always` binaries. Add this `bin/` directory to your path, or copy its contents to a directory on your path, and enjoy!

## Usage


## About This Implementation

This implementation has been tested on MacOSX but should work on any Unix-like platform, and with a little exta effort, maybe even on Windows.

## Performance

TODO

## Credits

D. J. Bernstein conceived the idea behind `redo` and wrote some notes at http://cr.yp.to/redo.html.

I first became interested in `redo` after looking at [Avery Pennarun](https://github.com/apenwarr/redo)'s Python implementation, and began using it in my own software projects. 

[Chris Forno](https://github.com/jekor) created a fantastic [on-camera](https://www.youtube.com/watch?v=zZ_nI9E9g0I) implementation of `redo` in Haskell which served as the inspiration for this implementation.
