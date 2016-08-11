.PHONY: all check

all: default.nix difftodo.cabal

default.nix: difftodo.cabal
	cabal2nix . > default.nix

difftodo.cabal: package.yaml
	hpack

check:
	nix-shell --run "cabal build"
