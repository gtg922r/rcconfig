#!/bin/sh

find ${1:-.} -name "*md" | while read FILE
do
    echo Converting \"$FILE\" from md to html5...
    pandoc -f markdown_github -t html5 --mathjax -H ~/.emacs.d/markdown/style_include.css  -o "${FILE%.*}".html "$FILE"
done
