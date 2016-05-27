#!/usr/bin/env bash

# Does any necessary Travis hacks. Currently Travis apparently does
# something very bizarre if you try to 'echo' something to a file, so
# let's not do that. Instead, run the commands explicitly
set -e
set -x
unset CDPATH

# Currently, the benchmarks take way too much memory and time to
# compile, so we clone a version of cereal in with a particular
# compile-time performance fix, which greatly enhances the memory
# use. This allows GHC 8.0+ to reliably build the benchmarks on
# Travis.
git clone https://github.com/GaloisInc/cereal.git
cd cereal && git reset --hard 4124881545bcc88951d1a10bb5e34e75520c380e && cd ..
echo "packages: cereal/" > cabal.project.local
