#cabal configure --enable-tests
cabal build -v0 && dist/build/causality-tests/causality-tests
