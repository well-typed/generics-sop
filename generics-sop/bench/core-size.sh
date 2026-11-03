#!/usr/bin/env bash
#
# Report how much Core a derived Generic instance costs, split into terms,
# types and coercions.
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

MODULES=(
  S10/GGP
  S10/TH
  S20/GGP
  S20/TH
  S40/GGP
  S40/TH
  S80/GGP
  S80/TH
)

# Optimization level for the measured modules. -O2 gives byte-identical Core
# for these: the passes it adds over -O are SpecConstr and Liberate case, and
# neither has anything to work on in a straight-line case tree. This does not
# affect how generics-sop itself is built, which is left to cabal.
GHC_OPT=${GHC_OPT:--O}

outputdir=$(mktemp -d)
trap 'rm -rf "$outputdir"' EXIT

# So that cabal exec below can resolve generics-sop itself, not just its deps.
cabal build generics-sop -v0

size_re='terms: ([0-9]+).*types: ([0-9]+).*coercions: ([0-9]+)'

printf '%-10s %10s %10s %12s\n' module terms types coercions

for m in "${MODULES[@]}"; do
  if ! passes=$(cabal exec -v0 -- ghc \
                  "$GHC_OPT" -fforce-recomp -dshow-passes \
                  -outputdir "$outputdir" -c "CoreSize/$m.hs" 2>&1); then
    printf '%s\n' "$passes" >&2
    echo "core-size.sh: ghc failed for CoreSize/$m.hs" >&2
    exit 1
  fi

  # Commas are thousands separators in GHC's output, not field separators.
  size=$(printf '%s\n' "$passes" \
         | grep -A1 'Result size of Tidy Core' | tail -1 | tr -d ',') || true

  if [[ ! $size =~ $size_re ]]; then
    echo "core-size.sh: no Tidy Core size for CoreSize/$m.hs, got: $size" >&2
    exit 1
  fi

  printf '%-10s %10s %10s %12s\n' \
    "$m" "${BASH_REMATCH[1]}" "${BASH_REMATCH[2]}" "${BASH_REMATCH[3]}"
done
