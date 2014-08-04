# difftodo

When I work on code, I tend to litter it with XXX, TODO, and FIXME comments.
Most of the time, I want to fix these comments before I submit my patch for
review.

This little utility:

 1. Finds all the comments you've edited or added with 'XXX' or 'TODO' in them
 2. Displays the output in a nice, human-readable way

Further, difftodo provides support for formatting the 'todos' in Emacs
compiler error style, so that you can easily jump to any outstanding comments
you've marked in your code.

## How to use it

### Extract todos from a diff

    diff -u base.py changed.py | difftodos

### Find todos in a bzr branch

First, you need to install this as a plugin, making sure Bazaar can find
`difftodo.commands.cmd_todo`. It has been a long time since I've used Bazaar,
so you'll have to figure it out yourself. If you do, please send me patches to
improve the documentation!

Then,

    bzr todo

That's it.

### Extract comments from a diff

I don't know why you'd want to do this, but...

    diff -u base.py changed.py | diffcomments


## What's supported

### Languages

Why do we have supported languages? Because we want to identify comments, and
for our output, we want to strip out the syntactic markers for comments so
that the resulting output is easier to read.

* Python

### Version control systems

You might wonder, 'why support version control at all'?

Well, we want to see what comments we've added or edited, which means that the
fundamental input is a unified diff. If you don't want to use version control,
you can just use that.

However, lots of people work on their changes in a branch, or in a forked
repository, and for them "work that I've done" means "the difference between
my branch and trunk" or "the difference between my repo and origin master" or
something similar. Rather than forcing them to make a diff themselves, we'd
like to provide convenience tools.

* Bazaar

### Editors & IDEs

It's good to be able to get a list of things to do right in your coding
environment, so you can jump straight to the thing that needs to be done.

* Emacs


## Dependency notes

We're porting this project to make better use of current Python packaging
technology. In the meantime, you should be aware that we depend on:

* bzr (for diff parsing and to integrate with bzr)
* testtools (for testing)
