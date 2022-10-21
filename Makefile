cups:
	curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

bootstrap:
	stack config set system-ghc --global true
	stack setup
	stack haddock --keep-going
	stack install hlint hpack
	hpack

build:
	stack build

run: build
	cd examples; stack ghc -- -fplugin=CoreDump.Plugin basic.hs; cd -

.PHONY: run build bootstrap cups