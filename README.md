# redo-haskell

An implementation of djb's [redo](http://cr.yp.to/redo.html) in [Haskell](https://www.haskell.org/). 

This implementation was inspired by [Chris Forno](https://github.com/jekor/redo)'s fantastic YouTube series [Haskell from Scratch](https://www.youtube.com/watch?v=zZ_nI9E9g0I), but has been improved upon in a few ways.

1. (TODO) `redo-ifcreate` is implemented, which rebuilds a target if a file appears
2. Target dependency meta-data is stored in a manner that should be immune to conflicts
3. (TODO) Improved colors and formatting on redo output to commandline
5. (TODO) `-jN` flag has been added to support parallel (faster) builds 
4. (TODO) A `redo-status` feature has been implemented which prints which dependencies of a target are up to date, and whose which are not.
5. (TODO) A `redo-init` feature has been added to inialize a repository for redo builds
6. (TODO) A `redo-destroy` deature has been added to remove all redo meta-data in a repository

This implementation has been tested on MacOSX but should work on any Unix-like platform, and with a little exta effort, maybe even on Windows.
