#!/usr/bin/env bash
#
# Report how much Core a derived Generic instance costs, split into terms,
# types and coercions. Pass --full to include the variants that only matter
# when working on the derivation itself.
#
# Every module under CoreSize/ contains exactly one datatype derived in exactly
# one way, so each row below is attributable to that single derivation, and the
# S10/S20 pair shows how the cost scales with constructor count.
#
# The types column is the one to watch: GHC counts a type synonym application
# as a single node rather than expanding it, so sharing subterms of the
# generated types between synonyms shows up directly as a smaller count.

set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

SIZES=(S10 S20 S40 S80)

# GGP and TH are the two derivations a user actually picks between, so they are
# what we report by default. The rest exist to attribute the cost of individual
# implementation choices within TH (THOnly as a metadata-free baseline, Inj and
# InjLocal for the injections-based variants), which is only of interest while
# working on the derivation itself: --full.
VARIANTS=(GGP TH)

case ${1-} in
  --full) VARIANTS+=(THOnly Inj InjLocal) ;;
  "")     ;;
  *)      echo "usage: ${BASH_SOURCE[0]##*/} [--full]" >&2; exit 2 ;;
esac

MODULES=()
for s in "${SIZES[@]}"; do
  for v in "${VARIANTS[@]}"; do
    MODULES+=("$s/$v")
  done
done

# Optimization level for the measured modules. -O2 gives byte-identical Core
# for these: the passes it adds over -O are SpecConstr and Liberate case, and
# neither has anything to work on in a straight-line case tree. This does not
# affect how generics-sop itself is built, which is left to cabal.
GHC_OPT=${GHC_OPT:--O}

# haskell-ci's jobs install a versioned cabal that is not on PATH as plain
# "cabal", and export its path as $CABAL, so take that when it is set. The
# compiler is whichever ghc cabal picks, which the caller selects via PATH:
# passing --with-compiler to cabal exec loses the package environment.
CABAL=${CABAL:-cabal}

outputdir=$(mktemp -d)
trap 'rm -rf "$outputdir"' EXIT

# So that cabal exec below can resolve generics-sop itself, not just its deps.
$CABAL build generics-sop -v0

size_re='terms: ([0-9]+).*types: ([0-9]+).*coercions: ([0-9]+)'

printf '%-14s %10s %10s %12s\n' module terms types coercions

for m in "${MODULES[@]}"; do
  if ! passes=$($CABAL exec -v0 -- ghc \
                  "$GHC_OPT" -fforce-recomp -dshow-passes \
                  -outputdir "$outputdir" -c "CoreSize/$m.hs" 2>&1); then
    printf '%s\n' "$passes" >&2
    echo "core-size.sh: ghc failed for CoreSize/$m.hs" >&2
    exit 1
  fi

  # GHC lays the size out with hang, so whether it breaks after the header
  # depends on how wide the numbers print; join both lines so that either
  # shape matches, then keep the first {..} alone, or a greedy match would
  # read the counts of the pass reported after this one. Commas are thousands
  # separators in that output, not field separators.
  size=$(printf '%s\n' "$passes" \
         | grep -A1 'Result size of Tidy Core' | tr -d ',\n' \
         | grep -o '{[^}]*}' | head -1) || true

  if [[ ! $size =~ $size_re ]]; then
    echo "core-size.sh: no Tidy Core size for CoreSize/$m.hs, got: $size" >&2
    exit 1
  fi

  printf '%-14s %10s %10s %12s\n' \
    "$m" "${BASH_REMATCH[1]}" "${BASH_REMATCH[2]}" "${BASH_REMATCH[3]}"
done
