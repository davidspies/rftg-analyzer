# RftG trace format

`rftg-trace` is a versioned JSON Lines stream produced by `engine/analyzer`.
Each non-empty line is one JSON object. The first line is always the header:

```json
{"event":"header","format":"rftg-trace","version":1,"decision_scope":"review"}
```

Version 1 supports these exhaustive event kinds:

- `header`: protocol identity and version;
- `log`: human-readable engine narration;
- `decision`: one rules-engine `make_choice` call and its answer;
- `draw`: the physical card selected for a player draw;
- `good`: the physical card placed face-down on a world;
- `refresh`: the discard pile was shuffled into the draw deck;
- `start_options`: the two physical start-world cards offered to a player;
- `mismatch`: a source-game expectation failed (the producer exits nonzero);
- `result`: final scores and winners.

Player numbers are stable, original pre-rotation identities. Card numbers are
physical deck indices in the emitting engine and are meaningful only within
the trace. Choice names are the exhaustive `CHOICE_*` names without their
prefix.

## Decisions

```json
{
  "event": "decision",
  "seq": 12,
  "player": 1,
  "type": "PAYMENT",
  "need": 0,
  "query": {
    "list": [12, 18],
    "special": [4],
    "args": [27, 0, 0]
  },
  "answer": {
    "rv": 0,
    "list": [12],
    "special": [4]
  },
  "state_digest": {
    "round": 4,
    "action": 3,
    "pool": 19,
    "deck": 91,
    "discard": 7,
    "goal_active": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    "goal_avail": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    "goal_most": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    "players": []
  }
}
```

`type` plus `query` is the Keldon `make_choice` wire question. `answer` is
exactly what is appended to the player's choice log. Together they map
directly to rftg2's `RawChoice` and `RawAnswer`; the `rftg-legacy-choice`
adapter owns conversion to and from the typed `Player` domain protocol.

`need` is `0` in a recorded trace. The live `keldon_match` transport emits
`need: 1` with an empty answer when its external player must reply on stdin.
This is an in-flight decision, not a complete replay record; complete-trace
consumers must reject pending decisions.

The decision object also contains the analyzer's existing presentation fields
(`offered`, `offered_special`, `options`, `predictions`, `chosen`, and
`state`). They are decorations for review clients. Replay consumers must use
`query`, `answer`, and `state_digest` as the authoritative machine fields.

For `ACTION`, `query.list` is empty because the engine supplies that array as
an output buffer rather than an offered-card list. `query.special` and all
answer arrays are always present, including when empty. Unsupported event
kinds, choice names, protocol names, or versions are errors.

`state_digest` is deliberately physical and presentation-free. Its three goal
arrays and each player's `goal_claimed` and `goal_progress` arrays have exactly
20 entries. Players are in original-player order; hands, tableaus, saved cards,
and the world IDs in `goods` are sorted physical IDs. One world ID appears in
`goods` for each good on that world.

The analyzer normally emits decisions only for the configured review player.
`--all-players` changes the header's `decision_scope` to `all` and emits every
engine decision; it does not change the event schema or replay behavior.

The analyzer and rftg2's live Keldon match link `engine/trace.c` for the
canonical fields, state digest, physical events, and result. The analyzer adds
its review-only presentation fields to the still-open decision object.
