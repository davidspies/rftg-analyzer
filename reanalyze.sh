#!/bin/bash
# Re-parse and re-analyze every cached BGA game, rewriting its analysis
# JSONL and meta.  This is the CLI equivalent of clicking "analyze" on
# each game in the UI -- run it after changing the parser, the engine,
# or cards.txt so the stored analyses pick up the new behavior.
#
# Usage: ./reanalyze.sh [user]   (default user: dspyz)
set -euo pipefail
cd "$(dirname "$0")"

USER_NAME="${1:-dspyz}"
mkdir -p data/scripts data/analysis

if [ -n "${RFTG_BGA_PARSER:-}" ]; then
  PARSER_BIN="$RFTG_BGA_PARSER"
else
  if ! (cd parser-hs && cabal build exe:rftg-bga-parser) \
        >/tmp/reanalyze.cabal 2>&1; then
    echo "CABAL-BUILD-FAIL: $(tail -1 /tmp/reanalyze.cabal | head -c 160)"
    exit 1
  fi
  if ! PARSER_BIN=$(cd parser-hs && cabal list-bin exe:rftg-bga-parser \
        2>/tmp/reanalyze.cabal); then
    echo "CABAL-LIST-BIN-FAIL: $(tail -1 /tmp/reanalyze.cabal | head -c 160)"
    exit 1
  fi
fi

shopt -s nullglob
games=(data/games/*.json)
if [ ${#games[@]} -eq 0 ]; then
  echo "no cached games in data/games/ -- sync some from the UI first"
  exit 0
fi

for g in "${games[@]}"; do
  t=$(basename "$g" .json)
  if ! "$PARSER_BIN" "$g" --user "$USER_NAME" \
        > "data/scripts/bga-$t.script" 2>/tmp/reanalyze.perr; then
    echo "[$t] PARSE-FAIL: $(tail -1 /tmp/reanalyze.perr | head -c 100)"
    continue
  fi
  if ! ./engine/analyzer "data/scripts/bga-$t.script" \
        > "data/analysis/bga-$t.jsonl" 2>/tmp/reanalyze.rerr; then
    echo "[$t] REPLAY-FAIL: $(tail -1 /tmp/reanalyze.rerr | head -c 100)"
    rm -f "data/analysis/bga-$t.jsonl"
    continue
  fi
  python3 - "$t" <<'PY'
import json, sys
t = sys.argv[1]
ev = [json.loads(l) for l in open(f"data/analysis/bga-{t}.jsonl") if l.strip()]
dec = sum(1 for e in ev if e.get("event") == "decision")
res = next((e for e in ev if e.get("event") == "result"), None)
mp = f"data/analysis/bga-{t}.meta.json"
try:
    meta = json.load(open(mp))
except FileNotFoundError:
    meta = {"source": "bga", "table_id": int(t)}
meta.update({"decisions": dec, "result": res})
json.dump(meta, open(mp, "w"))
PY
  echo "[$t] OK"
done
