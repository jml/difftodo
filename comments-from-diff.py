import sys

from bzrlib import patches
from difftodo import get_comments_from_diff


def main():
    phile = sys.stdin
    for comment in get_comments_from_diff(patches.parse_patches(phile)):
        print str(comment)


if __name__ == '__main__':
    main()
