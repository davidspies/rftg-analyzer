# Regression Tests

`fixtures/bga_games/` is an anonymized frozen copy of every BGA game that
was cached locally when this regression suite was created.  The fixture
payloads keep the raw BGA replay shape and original BGA table ids, but player
ids, player names, avatars, player channel ids, and countries are rewritten
before commit.
The BGA thumbnail lookup is dropped because it is not used by the parser and
contains unrelated player identifiers.

Run the Haskell parser replay regression with:

```sh
python3 -m unittest tests.test_cached_bga_games_haskell
```

The test builds `parser-hs` with Cabal, parses each fixture with
`rftg-bga-parser`, and replays the generated scripts through
`engine/analyzer --no-score`, writing generated scripts and analyzer output
to a temporary directory.

## Adding Fixtures

After fetching new cached games, anonymize them before committing:

```sh
python3 tools/anonymize_bga_fixtures.py --review-user <your-bga-username>
```

The review user is renamed to `Reviewer`; other players are renamed to
`Opponent 1`, `Opponent 2`, etc.  To verify that all fixtures match the
current anonymizer output:

```sh
python3 tools/anonymize_bga_fixtures.py --check
```
