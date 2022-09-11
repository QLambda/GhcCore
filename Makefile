cups:
	curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

bootstrap:
	stack setup 8.10.7

build: src
	stack build

run: build
	stack ghc -- -fplugin=CoreDump.Plugin examples/basic.hs