#!/usr/bin/env zsh

print -- "Compiling and building"
cabal install && \
byurakn.am clean && \
byurakn.am build
