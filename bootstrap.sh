#!/bin/sh -exu

# link sources into $HOME first
EMACS_D=$HOME/.emacs.d

./stow emacs

# this should probably just be make -C, lol
for p in \
  markdown-mode \
  helm \
  projectile \
  haskell-mode \
  scala-mode2 \
  yasnippet
do
  pushd ${EMACS_D}/${p}
    make
  popd
done

pushd ${EMACS_D}/flycheck
make init
make compile
popd
