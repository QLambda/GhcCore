# GhcCore

Currently we are using ghc-9.4.2



For chainging toolset:
```shell
ghcup tui
```

## Development environment setup

Check in ~/.vscode/settings.json    How to setup the development environment dependencies, then:

```shell
make bootstrap
```


HsToCore: https://serokell.io/blog/haskell-to-core

CoreLang: https://hackage.haskell.org/package/ghc-9.4.2/docs/src/GHC.Unit.Module.ModGuts.html#ModGuts

GHC Core Plugin: https://downloads.haskell.org/ghc/latest/docs/users_guide/extending_ghc.html?highlight=dynflags#manipulating-bindings
