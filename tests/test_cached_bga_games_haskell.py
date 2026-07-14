import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path

from server import haskell_parser
from server.app import parse_trace

ROOT = Path(__file__).resolve().parents[1]
FIXTURE_DIR = ROOT / "tests" / "fixtures" / "bga_games"
ANALYZER = ROOT / "engine" / "analyzer"
USER_NAME = os.environ.get("RFTG_REGRESSION_USER", "Reviewer")


def _last_line(text):
    lines = [line for line in text.splitlines() if line.strip()]
    return lines[-1] if lines else ""


class HaskellCachedBgaGameRegression(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.games = sorted(FIXTURE_DIR.glob("*.json"))
        if not cls.games:
            raise AssertionError(
                f"no cached BGA game fixtures found in {FIXTURE_DIR}")
        if not ANALYZER.exists():
            raise AssertionError(
                f"{ANALYZER} is missing; run `make -C engine` first")
        haskell_parser.executable()

    def test_cached_games_replay(self):
        failures = []
        with tempfile.TemporaryDirectory(prefix="rftg-haskell-regression-") as tmp:
            tmpdir = Path(tmp)
            for game in self.games:
                table_id = game.stem
                script = tmpdir / f"bga-{table_id}.script"
                analysis = tmpdir / f"bga-{table_id}.jsonl"

                try:
                    script_text = haskell_parser.parse_file(game, USER_NAME)
                except RuntimeError as exc:
                    failures.append(
                        f"{table_id}: Haskell parser failed: {exc}")
                    continue
                script.write_text(script_text)

                with analysis.open("w") as out:
                    replayed = subprocess.run(
                        [str(ANALYZER), "--no-score", str(script)],
                        cwd=ROOT,
                        stdout=out,
                        stderr=subprocess.PIPE,
                        text=True,
                    )
                if replayed.returncode != 0:
                    failures.append(
                        f"{table_id}: analyzer failed: "
                        f"{_last_line(replayed.stderr)}")
                    continue
                try:
                    parse_trace(analysis.read_text(), required_scope="review")
                except ValueError as exc:
                    failures.append(f"{table_id}: invalid analyzer trace: {exc}")

        if failures:
            self.fail("\n".join(failures))

    def test_parser_rejects_unknown_review_user(self):
        with self.assertRaisesRegex(RuntimeError, "review username"):
            haskell_parser.parse_file(self.games[0], "__not_a_player__")

    def test_parser_rejects_unknown_review_player_id(self):
        with self.assertRaisesRegex(RuntimeError, "review player id"):
            haskell_parser.parse_file(self.games[0], player_id=-1)

    def test_player_id_perspectives_replay_for_one_game(self):
        game = self.games[0]
        raw = json.loads(game.read_text())
        players = raw["logs"]["data"]["players"]
        failures = []

        with tempfile.TemporaryDirectory(prefix="rftg-perspective-") as tmp:
            tmpdir = Path(tmp)
            for player in players:
                player_id = int(player["id"])
                script = tmpdir / f"bga-{game.stem}-p{player_id}.script"
                analysis = tmpdir / f"bga-{game.stem}-p{player_id}.jsonl"

                try:
                    script_text = haskell_parser.parse_file(
                        game, player_id=player_id)
                except RuntimeError as exc:
                    failures.append(
                        f"{game.stem} p{player_id}: Haskell parser failed: {exc}")
                    continue
                script.write_text(script_text)

                with analysis.open("w") as out:
                    replayed = subprocess.run(
                        [str(ANALYZER), "--no-score", str(script)],
                        cwd=ROOT,
                        stdout=out,
                        stderr=subprocess.PIPE,
                        text=True,
                    )
                if replayed.returncode != 0:
                    failures.append(
                        f"{game.stem} p{player_id}: analyzer failed: "
                        f"{_last_line(replayed.stderr)}")
                    continue
                try:
                    parse_trace(analysis.read_text(), required_scope="review")
                except ValueError as exc:
                    failures.append(
                        f"{game.stem} p{player_id}: invalid analyzer trace: {exc}")

        if failures:
            self.fail("\n".join(failures))

    def test_rebel_pay_military_uses_strongest_matching_source(self):
        game = FIXTURE_DIR / "866716815.json"
        script_text = haskell_parser.parse_file(game, USER_NAME)
        script_lines = script_text.splitlines()
        place_line = script_lines.index('choice 1 PLACE "Rebel Warrior Race"')

        self.assertEqual(
            script_lines[place_line + 1],
            'choice? 1 PAYMENT none : "Rebel Alliance"',
        )

    def test_takeover_and_futile_defense_are_scripted(self):
        game = FIXTURE_DIR / "882143731.json"
        script_lines = haskell_parser.parse_file(game, USER_NAME).splitlines()

        self.assertIn(
            'choice? 1 TAKEOVER "Alien Booby Trap" : "Rebel Sneak Attack"',
            script_lines,
        )
        self.assertIn(
            'choice 0 DEFEND none : "Alien Booby Trap"',
            script_lines,
        )

    def test_non_windfall_discard_produce_is_scripted(self):
        game = FIXTURE_DIR / "882143731.json"
        script_lines = haskell_parser.parse_file(game, USER_NAME).splitlines()

        self.assertIn(
            'choice? 1 PRODUCE "Damaged Alien Factory" -1',
            script_lines,
        )
        self.assertIn(
            'choice? 1 DISCARD_PRODUCE "Primitive Rebel World" : '
            '"Damaged Alien Factory"',
            script_lines,
        )


if __name__ == "__main__":
    unittest.main()
