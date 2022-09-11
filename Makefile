cups:
	curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

bootstrap:
	stack setup 8.10.7

install: src/CoreDump/Plugin.hs
	cabal install --lib

run: install
	ghc -fplugin=CoreDump.Plugin examples/basic.hs