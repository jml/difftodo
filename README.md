# difftodo

Do you litter your code with `XXX`, `TODO`, and `FIXME` comments? Would you
like to actually _do_ those TODOs and _fix_ those FIXMEs? If so, `difftodo`
can help.

`difftodo` finds all the comments you've edited or added that are marked with
`XXX` or `TODO` and then displays the output in a nice, human-readable way.

## How to use it

### Extract todos from a diff

    diff -u base.py changed.py | diff-todo

### What do I need to do before merging this git branch?

    git diff master | diff-todo

### All todos in a file

    all-todos something.c

### All todos in your checkout

    git ls-files | xargs all-todos

## How to build

```
$ cabal configure
$ cabal build
$ cabal test
```

There's a `shell.nix` file too, if you're into that sort of thing.

## History

This was originally `bzr-todo`, and then became `difftodo`, and then got
rewritten in Haskell.

## Contributing

I'm very keen to make this better, but I'm also rather busy. Patches & issues
are welcome, but there's no SLA on replies.

If you find `difftodo` useful, then please let me know.
