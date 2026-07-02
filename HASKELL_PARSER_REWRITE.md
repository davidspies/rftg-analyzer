# Haskell Parser Rewrite Strategy

This document records the intended direction for replacing the current
Python BGA parser with a Haskell implementation. It exists so the rewrite
strategy survives context compaction and future sessions.

## Guiding Principle

Code architecture should reflect how we think about the problem. The code
should be written for humans first and the compiler second.

For this project, that means the parser should read like the game and the
translation problem:

```text
BGA JSON
  -> typed BGA events
  -> setup, rounds, and phase parsers
  -> typed Keldon script choices
  -> rendered analyzer script
```

The retired Python parser often mixed those layers in one pass over raw
notifications. That made real domain concepts appear as incidental flags,
pending dictionaries, string prefixes, and local special cases. The Haskell
parser should keep those concepts explicit.

## Tooling Choice

Use Cabal first.

Cabal is the standard Haskell build tool and is enough for this project:
one parser executable, one small internal library, ordinary dependencies,
and regression tests driven by the existing analyzer. Do not silently switch
to Stack if Cabal hits a local configuration or environment problem. Surface
the issue and discuss it.

Stack is acceptable later if there is a concrete reason, but it should not
be introduced just to hide a Cabal setup problem.

## Rewrite Style

This is a complete rewrite, not a line-by-line port of the retired Python
parser. The parser should be designed around typed domain concepts rather
than mirroring Python control flow.

The acceptance target is simple: the Haskell parser must emit scripts that
replay successfully through `engine/analyzer --no-score` for every cached
BGA fixture.

Current regression coverage:

- `tests/fixtures/bga_games/` contains 49 cached games.
- `python3 -m unittest tests.test_cached_bga_games_haskell` validates the
  Haskell parser against the same fixtures through `engine/analyzer
  --no-score`.

The local UI and `reanalyze.sh` now use the Haskell parser for BGA analysis.
The retired Python parser has been removed.

## Proposed Layout

Use a small Cabal project inside the repository, probably under
`parser-hs/`, with modules along these lines:

```text
parser-hs/
  rftg-bga-parser.cabal
  app/Main.hs
  src/Rftg/Bga/Json.hs
  src/Rftg/Bga/Event.hs
  src/Rftg/Bga/Normalize.hs
  src/Rftg/Keldon/Script.hs
  src/Rftg/Keldon/Render.hs
  src/Rftg/Parser/Game.hs
  src/Rftg/Parser/Setup.hs
  src/Rftg/Parser/Round.hs
  src/Rftg/Parser/Phase/Explore.hs
  src/Rftg/Parser/Phase/Develop.hs
  src/Rftg/Parser/Phase/Settle.hs
  src/Rftg/Parser/Phase/Consume.hs
  src/Rftg/Parser/Phase/Produce.hs
  src/Rftg/Parser/Phase/Discard.hs
```

Keep module boundaries aligned with domain boundaries. If a file does not
map to a concept in the design above, reconsider why it exists.

## Core Types

Avoid unstructured `Text` and `Int` once raw JSON has been decoded.

Expected wrapper types:

```haskell
newtype PlayerId = PlayerId Int
newtype Seat = Seat Int
newtype CardInstanceId = CardInstanceId Int
newtype GoodId = GoodId Int
newtype CardName = CardName Text
newtype StateId = StateId Int
```

Expected domain enums:

```haskell
data Phase
  = ActionSelect
  | Explore
  | Develop
  | Settle
  | Consume
  | Produce
  | Discard
  | GameOver

data BgaEvent
  = StateChanged StateId StateArgs
  | PhaseChoices PlayerId [Action]
  | CardPlayed PlayerId CardInstanceId CardName PaymentEvidence
  | CardsDiscarded PlayerId [CardInstanceId]
  | GoodProduced GoodId CardInstanceId
  | GoodConsumed GoodId
  | SearchRevealed PlayerId CardInstanceId CardName SearchMatchEvidence
  | SearchKept PlayerId CardInstanceId CardName
  | ScoreUpdated PlayerId Int
  | UnknownEvent Text Value

data KeldonEntry
  = Choice Seat KeldonChoice
  | OptionalChoice Seat KeldonChoice
  | Expect Seat Expectation
  | Draw Seat CardName
  | Header HeaderEntry

data KeldonChoice
  = ChooseAction Action Action
  | ChooseStart [CardName] [CardName] [CardName]
  | ChoosePlace CardName
  | DeclinePlace
  | ChoosePayment [CardName] [CardName]
  | ChooseSettlePower CardName
  | DeclineSettlePower
  | ChooseConsume CardName ConsumeArgs
  | ChooseGood [CardName]
  | ChooseConsumeHand CardName
  | ChooseTrade CardName
  | ChooseWindfall CardName
  | ChooseSearchType SearchCategory
  | ChooseSearchKeep SearchKeepIndex
  | ChooseDiscardPrestige CardName
  | DeclineTakeover
```

Names can change during implementation, but the important constraint is
that parser logic should manipulate typed choices, not rendered script
strings.

## Top-Level Shape

The main parser should be organized by the game:

```haskell
parseGame =
  parseSetup
  *> many parseRound
  *> parseGameEnd

parseRound =
  parseActionSelection
  *> optional parseExplore
  *> optional parseDevelop
  *> optional parseSettle
  *> optional parseConsume
  *> optional parseProduce
  *> optional parseDiscard
```

This sketch is directional, not a required parser-combinator API. The
important point is that phase boundaries should be the organizing principle.

## BGA Quirk Boundaries

BGA quirks should be explicit and localized.

Examples that should have named handling:

- BGA omits declined optional actions, while Keldon may ask for them.
- BGA omits the selected search category; infer a category consistent with
  revealed cards and Keldon gameplay.
- BGA can reveal whether search cards matched without naming the category.
- BGA may reuse good ids after consumption.
- BGA can emit metadata refreshes for active goods, such as Alien Oort kind
  changes, without producing a second good.
- BGA option value `0` for takeovers means Random; the resolved
  `gamedatas.takeovers` boolean is authoritative.
- BGA state changes are authoritative phase boundaries.
- Some BGA state packets use `active_player: 0` as a placeholder.

Do not bury these in ad hoc conditionals. Give each quirk a named function,
type, or phase-local rule.

## Phase Ownership

Each phase parser should own the temporary state and invariants for that
phase.

Examples:

- Search owns revealed cards, matched cards, rejected matches, and category
  inference.
- Settle owns placements, payments, extra-settle powers, upgrades, takeover
  declines, and settle-specific payment powers.
- Consume owns goods-consume grouping, hand consumes, free draws, trades,
  windfalls, and BGA/Keldon ordering differences.
- Produce owns windfall production and active-good tracking.
- Discard owns end-of-round and special discard choices.

Phase-local state should not leak into a global pile unless it is genuinely
cross-phase game state, such as hand ownership, tableau contents, active
goods, prestige, VP, or deck/card instance mapping.

## Invariants

Prefer fail-fast behavior. At phase boundaries and game end, assert that
there is no unresolved parser state.

Useful invariants include:

- no unresolved pending discards
- no unresolved search evidence
- no consume block with mixed goods and non-goods answers
- no duplicate incompatible good production for an active good id
- no consume of an inactive good id
- no unknown card instance id
- no unknown active player except documented BGA placeholder cases
- final hand/tableau/goods/prestige/VP expectations match BGA where known
- final score matches BGA for non-conceded games

When an invariant fails, fix the underlying translation or model. Do not
weaken the invariant to get the test green.

## Migration Plan

1. Add Cabal project scaffolding and a Haskell parser executable that can
   read a BGA fixture and emit a script.
2. Implement typed JSON decoding and event normalization.
3. Implement setup parsing and script header rendering.
4. Implement one phase at a time, validating against the fixture suite after
   each phase reaches useful coverage.
5. Add a Haskell regression test path that runs all 49 fixtures through the
   Haskell parser and `engine/analyzer --no-score`.
6. Keep the Python parser until the Haskell regression path is green on all
   fixtures. Completed.
7. Switch the default regression path to Haskell. Completed.
8. Remove or quarantine the Python parser after the Haskell parser is the
   trusted path. Completed.

## Non-Goals

- Do not redesign the analyzer script format during the parser rewrite.
- Do not add support for multiple BGA code versions until there is evidence
  we need it.
- Do not silently paper over Keldon/BGA rule mismatches. Surface ambiguous
  semantic issues before implementing compatibility behavior.
- Do not line-port Python helper structure into Haskell.

## Immediate Next Step

Start with Cabal scaffolding and typed JSON/event modules. Keep the first
milestone small: parse one fixture enough to emit a header and setup-related
script lines, then grow phase coverage while continuously running the
existing 49-game regression suite.
