# difftodo [![Build Status](https://travis-ci.org/jml/difftodo.svg)](https://travis-ci.org/jml/difftodo)

Do you litter your code with `XXX`, `TODO`, and `FIXME` comments? Would you
like to actually _do_ those TODOs and _fix_ those FIXMEs? If so, `difftodo`
can help.

`difftodo` finds all the comments you've edited or added that are marked with
`XXX` or `TODO` and then displays the output in a nice, human-readable way.

## How to use it

For most things, you just need to run `git todo`.

If you've got uncommitted, unstaged changes, `git todo` will show all TODOs
from those changes.

Otherwise, `git todo` will show all TODOs between your branch and `master`.

### What do I need to do before merging this git branch?

```
$ git todo origin/master...
git-todo/Main.hs:62:
  -- TODO: Take git diff flags as options

git-todo/Main.hs:73:
  -- TODO: Read this as a bytestring from the start

git-todo/Main.hs:83:
  -- TODO: Use gitlib
```

### What do I need to do before I can commit this?

```
$ git todo
git-todo/Main.hs:90:
  -- TODO: Factor out todo reporting
```

### Have I staged any TODOs?

```
$ git todo --cached
```

### What TODOs are left in my code base?

```
$ git todo --files
... too many to list here! ...
```

### Advanced usage

`git todo` tries to do the right thing. If you want more control, then you can
use the `all-todos` command, which shows all of the TODOs found in a
particular file.

e.g.

```
$ all-todos ../src-todo/branch-review.py
../src-todo/branch-review.py:11:
  # XXX: Incorporate this into difftodo
```

You can also use `diff-todo` to extract TODOs from a diff. This is more or
less how `git todo` works, so if you find yourself wanting a bit more control
over what `git todo` does you can just use `diff-todo`, e.g.

```
$ git diff v1.0.1...v1.0.0 | diff-todo
```

## Installing

Currently, we only support installing from source.

```
$ cabal configure
$ cabal install
```

You can also install the latest released version directly from Hackage:

```
$ cabal install difftodo
```

The `git todo` command assumes that `git` is available on the `PATH`.

## Building

```
$ cabal configure
$ cabal build
$ cabal test
```

The Cabal file is generated using `hpack`, and the `default.nix` file is
generated using `cabal2nix`. If you run `make`, these two generated files will
be updated.

## History

This was originally `bzr-todo`, and then became `difftodo`, and then got
rewritten in Haskell.

## Contributing

I'm very keen to make this better, but I'm also rather busy. Patches & issues
are welcome, but there's no SLA on replies.

If you find `difftodo` useful, then please let me know.
