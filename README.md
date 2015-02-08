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


## Dependencies

* [Pygments](http://pygments.org/) (wonderful library, we use it to parse
  diffs and every programming language under the sun)
* testtools (for testing)
