#!/usr/bin/env bash

if [[ "x$1" != "x" ]]; then
  echo "Enabling LLVM $1 build for binary-serialise-cbor"
  echo "package binary-serialise-cbor"                       > cabal.project.local
  echo "  ghc-options: -fllvm -pgmlo opt-$1 -pgmlc llc-$1"  >> cabal.project.local
fi
