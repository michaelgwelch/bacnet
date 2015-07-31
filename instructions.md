For my future self.

Use a sandbox `cabal sandbox init` or `cabal sandbox delete`

Switch version of Haskell Framework the activate-hs script is include 
  in the 7.8 version, which if activates is `/Library/Haskell/bin/activate-hs`,
  else it's `/Library/Haskell/ghc-7.8.3-x86_64/bin/activate-hs`

When starting with fresh working area and/or fresh sandbox do

*  `cabal install --depedencies-only --enable-tests`
*  `cabal configure --enable-tests`
*  `cabal build`
*  `cabal test`

Consider looking at more wanings

`cabal configure --enable-tests --ghc-options="-Wall -Werror"`
