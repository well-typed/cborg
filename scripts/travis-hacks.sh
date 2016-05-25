#!/usr/bin/env bash

# Does any necessary Travis hacks. Currently Travis apparently does
# something very bizarre if you try to 'echo' something to a file, so
# let's not do that. Instead, run the commands explicitly
set -e
set -x
unset CDPATH

git clone https://github.com/GaloisInc/cereal.git
cd cereal && git reset --hard 4124881545bcc88951d1a10bb5e34e75520c380e && cd ..
echo "packages: cereal/" > cabal.project.local
