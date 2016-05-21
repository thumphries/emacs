#!/bin/sh -exu

# link sources into $HOME first
EMACS_D=$HOME/.emacs.d

./stow emacs

for p in \
  markdown-mode \
  helm \
  projectile \
  haskell-mode
do
  pushd ${EMACS_D}/${p}
    make  
  popd
done
