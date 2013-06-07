#!/usr/bin/env zsh

for i in **/*.txt; do
  asciidoc -b xhtml11 -f asciidoc.conf -a disable-javascript -a data-uri -a stylesheet=$PWD/asciidoc-toc.css -a idprefix -o - $i | hxtoc -l 2 > ${i:r}.html
done
