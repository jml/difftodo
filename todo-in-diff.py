import sys

from bzrlib import patches
from difftodo import get_comments_from_diff

# XXX: Turn this into a Bazaar plugin
# XXX: Allow customization of TODO tags.

def main():
    phile = sys.stdin
    for comment in get_comments_from_diff(patches.parse_patches(phile)):
        if 'TODO' in comment or 'XXX' in comment:
            print str(comment)


if __name__ == '__main__':
    main()
