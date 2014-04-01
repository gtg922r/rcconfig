#!/bin/bash

# source: Stack Overflow, Richard Hansen (http://stackoverflow.com/a/9144984)

# make sure this script exits with a non-zero return value if the
# current directory is not in a svn working directory
svn info >/dev/null || exit 1

grep_string='^[?]'
outsidearray=()
if [ "$1" == "-i" ] || [ "$1" == "--delete-ignored" ]; then
    grep_string='^[I?]'
    printf "Searching for unversioned and ignored files...\n"
else
    printf "Searching for unversioned files (ignored files will be untouched)...\n"
fi

svn status --no-ignore | grep $grep_string | cut -c 9- |
# setting IFS to the empty string ensures that any leading or
# trailing whitespace is not trimmed from the filename

while IFS= read -r f; do
    # tell the user which file is being deleted.  use printf
    # instead of echo because different implementations of echo do
    # different things if the arguments begin with hyphens or
    # contain backslashes; the behavior of printf is consistent

    printf '%s\n' "Deleting ${f}..."
    # if rm -rf can't delete the file, something is wrong so bail
    rm -rf "${f}" || exit 1
done

