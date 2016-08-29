# Build is actually done with stack (https://haskellstack.org)
#
# This is mostly for generating the cabal and nix files from the package.yaml.

.PHONY: all check

all: default.nix difftodo.cabal

default.nix: difftodo.cabal
	cabal2nix . > default.nix

difftodo.cabal: package.yaml
	hpack

check:
	stack test
