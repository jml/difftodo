

## Revised strategy - use pygments

```python
import pygments
from pygments import lexers

eg_diff = open('example.diff', 'r').read()
pygments.lex(eg_diff, lexers.get_lexer_by_name('diff'))
```

Produces this output:


    [(Token.Generic.Heading, u'diff --git a/.gitignore b/.gitignore\n'),
     (Token.Generic.Heading, u'index 241973c..9bbeb13 100644\n'),
     (Token.Generic.Deleted, u'--- a/.gitignore\n'),
     (Token.Generic.Inserted, u'+++ b/.gitignore\n'),
     (Token.Generic.Subheading, u'@@ -1,3 +1,4 @@\n'),
     (Token.Generic.Inserted, u'+.env/\n'),
     (Token.Text, u' *.pyc\n'),
     (Token.Text, u' /_trial_temp/\n'),
     (Token.Text, u' /difftodo.egg-info/\n'),
     (Token.Generic.Heading,
      u'diff --git a/difftodo/_difftodo.py b/difftodo/_difftodo.py\n'),
     (Token.Generic.Heading, u'index 1618681..cda4adb 100644\n'),
     (Token.Generic.Deleted, u'--- a/difftodo/_difftodo.py\n'),
     (Token.Generic.Inserted, u'+++ b/difftodo/_difftodo.py\n'),
     (Token.Generic.Subheading,
      u'@@ -26,6 +26,10 @@ from something import patches\n'),
     (Token.Text, u' \n'),
     (Token.Text, u' from extensions import filter_none\n'),
     (Token.Text, u' \n'),
     (Token.Generic.Inserted, u'+# A comment\n'),
     (Token.Generic.Inserted, u'+\n'),
     (Token.Generic.Inserted,
      u'+# XXX: Use Pygments to do all of our lexing for us.\n'),
     (Token.Generic.Inserted, u'+\n'),
     (Token.Text, u' \n'),
     (Token.Text, u' class Comment(object):\n'),
     (Token.Text, u'     """A comment block in a Python source file."""\n')]


* [ ] Use `Token.Generic.Heading` to identify filenames
  * [ ] But check for other kinds of diff
* [ ] Find `Token.Generic.Inserted` and `Token.Generic.Text`
  * [ ] Possibly use `pygments.filters.TokenMergeFilter` to make this easier
* [ ] Combine adjacent 'inserted' & 'text' so we see added and remaining
* [ ] Return a list of these, each with filename and ideally line number annotations
* [ ] For each, find a pygments.lexer (`lexers.get_lexer_for_filename`, then `guess_lexer`. Failure is raised `pygments.util.NoClassFound`)
* [ ] Use `TokenMergeFilter` and probably a custom version of `CodeTagFilter` to identify TODO comments
* [ ] Format these nicely and present to end user

Line numbers will be a challenge.

After this we will get 'for free' a Haskell comment parser, and be able to
drop our dependency on bzr.

* [ ] Move emacs integration to its own directory
* [ ] Use entry points for command-line scripts
* [ ] Make sure there's a core script that just takes a diff
* [ ] Pluggable output format
* [ ] Config file for custom tagging

