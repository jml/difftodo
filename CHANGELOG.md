# Change log


## 0.1.0 (2016-08-11)

Complete rewrite in Haskell. Mostly command-line compatible with Python
version.

Version numbering has been reset to 0.1.0 in order to communicate the
instability of the library APIs.

Tags can no longer be specified on the command line.

## Python version

No longer supported. Available at the git tag `python-v1.0.0`.

### 1.0.0 (2015-02-08)

Complete rewrite to use Pygments for diff and code parsing, providing support
for many, many more languages and allowing `difftodo` to be used with version
control systems other than Bazaar.

* All support for Bazaar has been removed
* None of the old APIs work
* Emacs integration has been deleted (but look out for it in a new repo)
* Examples in documentation refer to git
* `difftodos` command-line tool renamed to `diff-todo`
* New tool `all-todos` to show all todo comments in files
* `diffcomments` command-line tool deleted
* Tags (e.g. "FIXME", "XXX", "TODO") can now be specified on the command-line
* Tests now use `testtools` rather than `bzrlib.tests`

### 0.0.2

bzr- & Python-specific version. No longer supported.
