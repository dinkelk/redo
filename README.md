# redo

An implementation of djb's [redo](http://cr.yp.to/redo.html) in [Haskell](https://www.haskell.org/). What is redo? To quote [Chris Forno](https://github.com/jekor):

> Redo allows you to rebuild files from source files when they've changed. It's simpler than other build systems such as Make or SCons and more generic than language-specific build systems such as Cabal or Apache Ant. Redo gains its power and simplicity by leveraging other tools (in the Unix tradition). Build scripts for redo are simply shell scripts that follow a few conventions.

## Installation

To install redo, clone this repository and run:

    ./do build

in the top level directory. A `bin/` directory will be created with the `redo`, `redo-ifchange`, and `redo-ifcreate` binaries. Add this `bin/` directory to your path or copy its contents to a directory on your path and enjoy!

## Usage

TODO

## About This Implementation

This implementation was inspired by [Chris Forno](https://github.com/jekor/redo)'s fantastic YouTube series [Haskell from Scratch](https://www.youtube.com/watch?v=zZ_nI9E9g0I), but has been improved upon in a few ways.

1. (TODO) `redo-ifcreate` is implemented, which rebuilds a target if a file appears
2. Target dependency meta-data is stored in a manner that should be immune to conflicts
3. (TODO) Improved colors and formatting on redo output to commandline
5. (TODO) `-jN` flag has been added to support parallel (faster) builds 
4. (TODO) A `redo-status` feature has been implemented which prints which dependencies of a target are up to date, and whose which are not.
5. (TODO) A `redo-init` feature has been added to inialize a repository for redo builds
6. (TODO) A `redo-destroy` deature has been added to remove all redo meta-data in a repository

This implementation has been tested on MacOSX but should work on any Unix-like platform, and with a little exta effort, maybe even on Windows.

## Performance

TODO

## Credits

D. J. Bernstein conceived the idea behind `redo` and wrote some notes at http://cr.yp.to/redo.html.

I first became interested in `redo` after looking at [Avery Pennarun](https://github.com/apenwarr/redo)'s Python implementation, and began using it in my own software projects. 

[Chris Forno](https://github.com/jekor) created a fantastic [on-camera](https://www.youtube.com/watch?v=zZ_nI9E9g0I) implementation of `redo` in Haskell which served as the starting place for this implementation.
