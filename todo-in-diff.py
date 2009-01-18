import sys

from bzrlib.patches import parse_patches
from difftodo import get_comments_from_diff, todos_from_comments

# XXX: Turn this into a Bazaar plugin
# XXX: Allow customization of TODO tags.

# TODO: oaeuah

def main():
    patches = parse_patches(sys.stdin)
    comments = get_comments_from_diff(patches)
    tags = ('XXX', 'TODO')
    number = -1
    for number, todo in enumerate(todos_from_comments(comments, tags)):
        print todo
    print "Things to do: %s" % (number + 1)


if __name__ == '__main__':
    main()
