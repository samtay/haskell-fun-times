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

  pandoc -s -o "$topdir.html" "$mdFiles"
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

__build_ffp() {
  __build "from-first-principles"
}

main() {
  local topdirs="from-first-principles learn-you-a-haskell cis-194 typeclassopedia"
  git checkout gh-pages
  git reset --hard master
  for topdir in "$topdirs"; do
    __build $topdir
    sed -i -e "s/.\/$topdir/$topdir.html/g" README.md
  done
  pandoc -s -o "index.html" README.md
  git add "index.html $topdirs"
  git commit -m 'Compiled haskell content into somewhat readable html'
  git push
}

main "$@"
