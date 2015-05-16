#!/usr/bin/env bash

# This works:
cabal exec -- ghci test/Spec.hs -fdefer-typed-holes

# wheras this doesn't:
cabal exec -- sensei-web test/Spec.hs -fdefer-typed-holes
