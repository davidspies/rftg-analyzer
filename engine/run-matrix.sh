#!/bin/bash
# Round-trip test: generate AI game scripts and verify the analyzer
# replays each to the identical final score.
cd "$(dirname "$0")"
pass=0; fail=0
for cfg in "$@"; do
  ./genscript $cfg > t.script 2> t.result
  ./analyzer --no-score t.script > t.replay 2> t.err; rc=$?
  rep=$(tail -1 t.replay | python3 -c "import json,sys; d=json.load(sys.stdin); print(','.join(f\"{p['name']}:{p['vp']}\" for p in sorted(d['players'],key=lambda x:x['name'])))" 2>/dev/null)
  exp=$(grep result t.result | sed 's/result [0-9]* "\(.*\)": \([0-9]*\) VP.*/\1:\2/' | sort | paste -sd,)
  if [ -n "$rep" ] && [ "$rep" == "$exp" ]; then st=MATCH; pass=$((pass+1)); else st=DIFFER; fail=$((fail+1)); fi
  echo "[$cfg] rc=$rc $st $(grep -v 'could not avoid' t.err | head -c 80)"
done
echo "== $pass pass, $fail fail"
