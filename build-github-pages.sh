#!/usr/bin/env bash

# usage: __build topdir
__build() {
  local topdir=$1

  ## compile .hs files to .md files
  __compileHs $topdir

  ## aggregate all .md files
  local mdFiles=""
  for mdFile in $(find -type f -name "*.md" -path "./$topdir*" | sort); do
    # poor mans sorting
    if [[ "$mdFile" == "./$topdir/$(basename $mdFile)" ]]; then
      mdFiles="$mdFile $mdFiles"
    else
      mdFiles="$mdFiles $mdFile"
    fi
  done

  # for some reason this doesn't work: pandoc -s -o "$topdir.html" "$mdFiles"
  echo "$mdFiles" | pandoc -s -o "$topdir.html"
}

# compiles haskell files
# cp *.hs -> *.md and add gfm syntax hints
# usage: __compileHs topdir
__compileHs() {
  local topdir=$1

  for hsfile in $(find -type f -path "./$topdir*.hs" -not -path "*stack-work*" | sort); do
    cp $hsfile "$hsfile.md"
    sed -i '1i ```haskell' "$hsfile.md"
    echo '```' >> "$hsfile.md"
  done
}

# cleanup *.hs.md files, git stuff
__cleanup() {
  for f in $(find -type f -name "*.hs.md"); do
    rm $f
  done
  git checkout README.md
}

main() {
  local -a topdirs=(from-first-principles learn-you-a-haskell cis-194 typeclassopedia)

  # sync with master
  git checkout gh-pages
  git reset --hard master

  # build HTML content
  for topdir in ${topdirs[@]}; do
    __build $topdir
    sed -i -e "s/.\/$topdir/$topdir.html/g" README.md
  done
  pandoc -s -o "index.html" README.md

  # send off to github
  for f in ${topdirs[@]} + "index"; do
    git add "$f.html"
  done
  git commit -m 'Compiled haskell content into somewhat readable html'
  git push -f

  __cleanup
  git checkout master
}

main "$@"
