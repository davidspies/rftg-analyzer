# RftG analyzer engine

Replays a scripted Race for the Galaxy game through Keldon Jones' engine
(vendored, patched, in `../engine-src/`) and asks his neural-network AI
to score every available option at each decision point of the player
under review.  Scores approximate the player's win probability.

## Build

    make          # builds ./analyzer and ./genscript

Card database and trained networks live in `data/` (path compiled in
via RFTGDIR).

## Usage

    ./analyzer [-v] [--no-score] game.script   >  analysis.jsonl

- `--no-score`: replay only (fast, used for validation)
- `-v`: trace engine narration and every choice ask to stderr

Output is JSON-lines: `decision`, `log`, `mismatch`, and `result`
events.  Each `decision` carries the offered options, all scored
candidates (`options[].score`), the actually-chosen answer, and a full
visible-state snapshot.

## Script format

Text lines; `#` comments; quoted strings for card names.

Header (must precede draws/choices):

    players 2            # 2-5
    expanded 1           # 0 base, 1 TGS, 2 RvI, 3 BoW
    advanced 1           # 2P advanced game
    promo 1              # include New Worlds promo start worlds
    goals 1              # goals enabled
    takeovers 1          # takeovers enabled
    seed 42              # randomness for hidden-info fills
    review 0             # player index whose decisions get scored
    score 1              # 0 = replay only
    name 0 "dspyz"
    goal "Budget Surplus"        # repeat for each active goal

Body, in per-player order (interleaving between players is free):

    draw 0 "Alpha Centauri"      # player 0's next draw; first draw is
                                 # the start world
    draw 0 ?                     # hidden draw (opponent), random fill
    expect 0 hand 4              # checked just before player 0's next
    expect 0 vp 12               # choice; vp/hand/tableau/prestige
    choice 0 ACTION 5 -1        # engine action codes
    choice 0 DISCARD "Card A" "Card B"     # or: none
    choice 0 PLACE "Mining World"          # or: none
    choice 0 PAYMENT "Card A" : "Contact Specialist"   # cards : specials
    choice 0 TRADE "Mining World"
    choice 0 CONSUME "Galactic Bazaar" 1   # card + power index;
                                           # none; or prestige
    choice 0 CONSUME_HAND "Card A"
    choice 0 GOOD "Mining World"           # world(s) whose good is used
    choice 0 WINDFALL "Mining World"
    choice 0 PRODUCE "Card" 0
    choice 0 DISCARD_PRODUCE "Card" : "World"          # or: none
    choice 0 SAVE "Card"
    choice 0 DISCARD_PRESTIGE "Card"       # or: none
    choice 0 UPGRADE "New World" : "Old World"
    choice 0 LUCKY 3
    choice 0 ANTE "Card"                   # or: none
    choice 0 KEEP "Card"
    choice 0 SEARCH_TYPE 2
    choice 0 SEARCH_KEEP 1
    choice 0 OORT_KIND 1

Not yet supported: TAKEOVER target selection, DEFEND,
TAKEOVER_PREVENT (the analyzer dies loudly if a script contains them).

## Hidden information

Opponent draws are scripted as `?` and filled randomly from cards not
otherwise spoken for.  Cards an opponent later reveals (plays) must be
scripted into their draw stream at or before the reveal; the BGA parser
assigns them retroactively.  Face-down goods are anonymous: the engine
draws unreserved random cards for them and swaps a card back out if a
scripted draw later needs it.

## Validation

    ./run-matrix.sh "-p 2 -e 1 -r 7" "-p 3 -e 2 -r 5" ...

generates AI-vs-AI games with `genscript` (which records full scripts
including per-choice `expect` assertions) and verifies the analyzer
replays each to the identical final score.

## Engine patches (engine-src/)

- `ai.c`: `ai_option_hook` (per-option score recording in the ACTION
  and PLACE handlers), `ai_eval_choice` (generic candidate scorer).
- `engine.c`: `draw_hook`; campaign draws resolve lazily by design and
  rotate with players; random draws avoid scripted-demand starvation;
  good-swap recovery; `flip_world` routed through `campaign_draw`.
- `init.c`: lazy campaign entries (`-2`) instead of failing on
  duplicate designs.
- `rftg.h`: declarations; `campaign_status.order_d`.
