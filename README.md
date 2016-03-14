# redo

`redo` is like `make`, but better.

Like `make`, `redo` allows you to rebuild source files when they have changed. But unlike `make`, `redo` is simple, easy to learn, and flexible. `redo` exhibits its power the Unix way, by leveraging other tools. Redo build scripts are simply shell scripts which follow a few conventions. If you don't like shell, redo also supports any language that can be run with a shebang (`!#`).

While I wrote this implementation of `redo`, credit for the original design goes to D. J. Bernstein, who published as a set of notes on the program on his [website]( http://cr.yp.to/redo.html). Since djb has never released his version of `redo`, a few people have created and published their own implementations. Before writing this implementation, I used [apenwarr's python implementation](https://github.com/apenwarr/redo) on a daily basis in my projects. Later, I found [jekor's video series] (http://www.youtube.com/playlist?list=PLxj9UAX4Em-Ij4TKwKvo-SLp-Zbv-hB4B) on implementing redo from scratch in [Haskell](https://www.haskell.org/). I was intrigued at the prospect of implementing an elegant program in an elegant language, and so I began implementing my own version of `redo` in Haskell. The end result is this version of `redo`, which looks and feels very similiar to apenwarr's implementation, but fixes some annoying bugs. 

I intend to continue maintaining and improving this implementation of `redo` for the forseeable future. If you encounter any problems, feel free to submit a pull request or post an issue.

## Installation

To install redo, first make sure you have [GHC](https://www.haskell.org/ghc/) installed. For some, it may be easier to just install the whole [Haskell Platform](https://www.haskell.org/platform/).

Next, clone this repository and run:

    ./do

in the top level directory. A `bin/` directory will be created with the `redo`, `redo-ifchange`, `redo-ifcreate`, and `redo-always` binaries. Add this `bin/` directory to your path, or copy its contents to a directory on your path, and enjoy!

## Usage

`redo` is a handful of very sharp tools which you can use 

## About This Implementation

This implementation has been tested on MacOSX but should work on any Unix-like platform.

## Compared to Other Implementations

## Credits

D. J. Bernstein conceived the idea behind `redo` and wrote some notes at http://cr.yp.to/redo.html.

I first became interested in `redo` after looking at [Avery Pennarun](https://github.com/apenwarr/redo)'s Python implementation, and began using it in my own software projects. 

[Chris Forno](https://github.com/jekor) created a fantastic [on-camera](https://www.youtube.com/watch?v=zZ_nI9E9g0I) implementation of `redo` in Haskell which served as the inspiration for this implementation.
