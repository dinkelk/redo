# redo

`redo` is like `make`, but better.

Like `make`, `redo` allows you to rebuild source files when they have changed. But unlike `make`, `redo` is simple, easy to learn, and flexible. `redo` exhibits its power the Unix way, by leveraging other tools. Build scripts for `redo` are simply shell scripts which follow a few conventions. If you don't like shell, `redo` also supports any language that can be run with a shebang (`!#`), ie. python, perl, etc.

While I wrote this implementation of `redo`, credit for the original design goes to D. J. Bernstein, who published its design in a set of notes on his [website]( http://cr.yp.to/redo.html). Since djb has never released his version of `redo`, a few people have created and published their own implementations. Before writing this implementation, I used [Avery Pennarun's python implementation](https://github.com/apenwarr/redo) on a daily basis in my projects. Later, I found [Chris Forno's video series](http://www.youtube.com/playlist?list=PLxj9UAX4Em-Ij4TKwKvo-SLp-Zbv-hB4B) on implementing redo from scratch in [Haskell](https://www.haskell.org/). I was intrigued at the prospect of implementing an elegant program in an elegant language, and so I began implementing my own version of `redo` in Haskell. The end result is this version of `redo`, which looks and feels very similiar to apenwarr's implementation, but fixes some annoying bugs, and may run a bit faster.

I intend to continue maintaining and improving this implementation of `redo` for the forseeable future. If you encounter any problems, feel free to submit a pull request or post an issue. I continue to use this version of `redo` every day.

## Compilation and Installation

To build `redo`, first make sure you have the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) installed.

Next, clone this repository and run:

    ./do

in the top level directory. A `bin/` directory will be created with the `redo`, `redo-ifchange`, `redo-ifcreate`, and `redo-always` binaries. Add this `bin/` directory to your path, or copy its contents to a directory on your path, and enjoy!

## Documentation

I try to keep this implementation compatible with [Avery Pennarun](https://github.com/apenwarr/redo)'s Python implementation, plus some bug fixes, and extra features documented in `redo -h`. Thus, the best documentation on `redo` that exists is Avery's [documentation](https://redo.readthedocs.io/en/latest/) via readthedocs.org

Note that `redo`'s database is stored in `~/.redo`. If `redo` ever gets confused for some reason, you can always reset its state by cleaning your project, running `rm -r ~/.redo`, and running your `redo` command again.

## Credits

D. J. Bernstein conceived the idea behind `redo` and wrote some notes at [http://cr.yp.to/redo.html](http://cr.yp.to/redo.html).

I first became interested in `redo` after using [Avery Pennarun](https://github.com/apenwarr/redo)'s Python implementation to build my own projects. 

[Chris Forno](https://github.com/jekor) created a fantastic [on-camera](https://www.youtube.com/watch?v=zZ_nI9E9g0I) implementation of `redo` in Haskell which served as the inspiration for this implementation.
