# Experimental Stuff with Haskell

This repository contains some experimental stuff while learning fundamentals of Haskell.

Make sure hspec is installed
`cabal update && cabal install hspec`

Next install and register the package in the package database.
`cabal install --enable-tests --only-dependencies`

Prepare to build the package, build the package and run the tests suite.
`cabal configure --enable-tests && cabal build && cabal test`

## TODOs
- there is a `IsSplittedHand` field in `Hand` and also a `SplitCount` --> merge these data