cups:
	curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

bootstrap:
	stack config set system-ghc --global true
	stack setup
	stack haddock --keep-going
	stack install hlint

build: src
	stack build

run: build
	stack ghc -- -fplugin=CoreDump.Plugin examples/basic.hs