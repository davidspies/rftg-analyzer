# RftG Analyzer

Review your BoardGameArena **Race for the Galaxy** games with Keldon
Jones' neural-network AI: step through every decision you made and see
the AI's estimated win probability for each available option.

## Components

| Path | What |
|------|------|
| `engine-src/` | Keldon's RftG engine + AI — submodule of [davidspies/rftg](https://github.com/davidspies/rftg) (`analyzer` branch; diff vs upstream [bnordli/rftg](https://github.com/bnordli/rftg) is one commit) |
| `engine/` | `analyzer` (scripted replay + option scoring), `genscript` (test-script generator), trained networks in `data/` |
| `parser-hs/` | Haskell BGA log parser |
| `server/` | local web server and BGA fetcher |
| `ui/` | single-page review interface |

## Install

1. Install system dependencies.

   On Debian/Ubuntu:

   ```sh
   sudo apt update
   sudo apt install \
     git build-essential python3 curl ca-certificates xz-utils \
     libffi-dev libgmp-dev libncurses-dev pkg-config
   ```

   `libgmp-dev` is needed by the Haskell toolchain used to build the
   BGA parser.

2. Install GHC 9.6 or newer and Cabal.

   `ghcup` is the usual way to install current Haskell tools on Linux.

   ```sh
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
   ```

   Follow the installer prompts, then open a new shell and verify:

   ```sh
   ghc --numeric-version
   cabal --numeric-version
   ```

   Some distro `ghc` packages are too old for this project. If you use
   distro packages instead of `ghcup`, make sure `ghc --numeric-version`
   reports 9.6 or newer.

3. Clone the repository and fetch the engine submodule.

   ```sh
   git clone https://github.com/davidspies/rftg-analyzer.git
   cd rftg-analyzer
   git submodule update --init
   ```

4. Build the analyzer engine.

   ```sh
   cd engine
   make
   cd ..
   ```

5. Build the BGA parser.

   ```sh
   cabal build all
   ```

   The root `cabal.project` points Cabal at `parser-hs/`.

6. Start the local server.

   ```sh
   python3 server/app.py
   ```

7. Open <http://127.0.0.1:8421/> and click **New demo game** to explore
   the interface on an AI-vs-AI game.

## Quick start

```sh
git submodule update --init      # engine source
cd engine && make && cd ..
python3 server/app.py            # http://127.0.0.1:8421/
```

Click **New demo game** to explore the interface on an AI-vs-AI game.

## Tests

```sh
python3 -m unittest tests.test_cached_bga_games_haskell
```

The cached-game regression fixtures live in `tests/fixtures/bga_games/`.
The test replays each fixture with `engine/analyzer --no-score`.

## Connecting your BGA account

1. `cp config.example.json config.json`
2. Log in to boardgamearena.com, copy the `Cookie` request header from
   dev tools (F12 → Network → any request), paste into `config.json`.
3. Ensure `cabal` is on `PATH`; BGA analysis builds and runs the Haskell
   parser from `parser-hs/`.
4. In the UI, click **Sync BGA games**, then a table to analyze it.

## Status

- ✅ Engine replay + scoring: validated on 40+ generated games across
  base/TGS/RvI/BoW, 2–5 players, advanced 2P, New Worlds promo.
- ✅ UI with demo games and real BGA games.
- ✅ BGA fetcher + Haskell parser: all cached games (2P advanced and 3P,
  including a conceded game) replay end-to-end with per-round state
  validation and exact final-score matches against BGA's records.
- 🚧 Takeover-enabled games are not yet scriptable
  (TAKEOVER/DEFEND/TAKEOVER_PREVENT).
  Takeover/defend choices not yet scripted. Debugging loop:
  `cabal exec rftg-bga-parser -- data/games/<t>.json --user <name> > s.script`
  then `engine/analyzer --no-score -v s.script` vs
  `python3 server/timeline.py data/games/<t>.json`.
- Win-probability caveats: scores come from `eval_game`, which is the
  AI's value function — win probability plus small VP-progress shaping
  terms, so values can slightly exceed 1. Comparisons within one
  decision are what matter. Games with hidden opponent hands are
  reconstructed with random fill consistent with revealed information.

## License

RftG Analyzer is licensed under the GNU General Public License,
version 2 or later. See `LICENSE`.
