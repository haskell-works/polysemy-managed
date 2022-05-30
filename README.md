# polysemy-managed

[![Binaries](https://github.com/haskell-works/polysemy-managed/actions/workflows/haskell.yml/badge.svg)](https://github.com/haskell-works/polysemy-managed/actions/workflows/haskell.yml)

Provided the `Managed` effect which grants the effect stack a `MonadResource` for easy compatibility with libraries
that expose an API that uses the `MonadResource` constraint or the `ResourceT` transformer in its effect type.

See `Polysemy.ManagedSpec` for examples.
