# redo-haskell

An implementation of djb's [redo](http://cr.yp.to/redo.html) in [Haskell](https://www.haskell.org/). 

This implementation was inspired by [Chris Forno](https://github.com/jekor)'s fantastic YouTube series [Haskell from Scratch](https://www.youtube.com/watch?v=zZ_nI9E9g0I), but has been improved upon in a few ways.

1. `redo-ifcreate` is implemented (TODO)
2. Target dependency meta-data is stored in a manner that should be immune to conflicts
3. `redo-init` and `redo-destroy` features have been added (TODO)

This implementation has been tested on MacOSX but should work on any Unix-like platform, and maybe even Windows.
