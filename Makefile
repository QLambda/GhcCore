cups:
	curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

bootstrap:
	stack setup 8.10.7
	stack haddock --keep-going
	cd /tmp; stack install hlint; cd --
	echo "Install language server as specified in README.md"

build: src
	stack build

run: build
	stack ghc -- -fplugin=CoreDump.Plugin examples/basic.hs