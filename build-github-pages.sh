#!/usr/bin/env bash

# usage: build
build() {
  ## compile .hs files to .md files
  compileHs

  ## aggregate all .md files
  local content=""
  local sep=$'\n\n'
  for mdFile in $(find -type f -name "*.md" -path "./$__topdir*" | sort); do
    # poor mans sorting
    if [[ "$mdFile" == "./$__topdir/$(basename $mdFile)" ]]; then
      content="$(cat $mdFile)$sep$content"
    else
      content="$content$sep$(cat $mdFile)"
    fi
  done

  # for some reason this doesn't work: pandoc -s -o "$__topdir.html" "$mdFiles"
  rm -f "$__topdir.html"
  echo "$content" | pandoc --read=markdown_github --standalone --output "$__topdir.html"
}

# compiles haskell files
# cp *.hs -> *.md and add gfm syntax hints
# usage: compileHs
compileHs() {
  for hsfile in $(find -type f -path "./$__topdir*.hs" -not -path "*stack-work*" | sort); do
    cp $hsfile "$hsfile.md"
    sed -i '1i ```haskell' "$hsfile.md"
    echo '```' >> "$hsfile.md"
  done
}

# cleanup *.hs.md files, git stuff
cleanup() {
  for f in $(find -type f -name "*.hs.md"); do
    rm $f
  done
  git checkout README.md
}

main() {
  set -eo pipefail
  local -a topdirs=(from-first-principles learn-you-a-haskell cis-194 typeclassopedia)

  # sync with master
  git checkout gh-pages
  git reset --hard master

  # build HTML content
  for __topdir in ${topdirs[@]}; do
    build
    sed -i -e "s/.\/$__topdir/$__topdir.html/g" README.md
  done
  pandoc -s -o "index.html" README.md

  # send off to github
  for f in "${topdirs[@]}"; do
    git add "$f.html"
  done
  git add "index.html"
  git commit -m 'Compiled haskell content into somewhat readable html'
  git push -f

  cleanup
  git checkout master
}

main "$@"
