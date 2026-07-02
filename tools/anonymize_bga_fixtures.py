#!/usr/bin/env python3
"""Anonymize cached BGA replay fixtures.

The parser tests need raw-ish BGA payloads because private per-player
replay state is spread through several objects.  This tool keeps that
shape intact while preserving table ids and replacing player ids, names,
avatars, and country objects with deterministic fixture-local values.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any


DEFAULT_FIXTURE_DIR = Path("tests/fixtures/bga_games")
MARKER = "_anonymized"
MARKER_VERSION = 1
PLAYER_ID_BASE = 910_000_000
UNKNOWN_PLAYER_CHANNEL_ID = PLAYER_ID_BASE + 999_999
BGA_PLAYER_CHANNEL_RE = re.compile(r"/player/p(\d+)")
BGA_TABLE_CHANNEL_RE = re.compile(r"/table/t(\d+)")
ANON_PLAYER_LABEL_RE = re.compile(r"(Reviewer|Player [1-9][0-9]*|Opponent [1-9][0-9]*)\Z")


@dataclass(frozen=True)
class ReplacementPlan:
    table_id: int
    int_values: dict[int, int]
    str_values: dict[str, str]
    substrings: dict[str, str]


def main() -> int:
    args = parse_args()
    files = fixture_files(args.paths)
    if not files:
        raise SystemExit("no fixture files matched")

    changed = []

    for path in files:
        doc = load_json(path)
        marked = MARKER in doc
        if marked and not (args.force or args.check or args.dry_run):
            continue

        table_id = fixture_table_id(path, doc)
        plan = replacement_plan(
            doc,
            table_id,
            args.review_user,
            preserve_labels=marked and args.review_user is None,
        )
        anonymized = rewrite(doc, plan)
        anonymized[MARKER] = {
            "tool": "tools/anonymize_bga_fixtures.py",
            "version": MARKER_VERSION,
        }

        rendered = json.dumps(anonymized, indent=1, ensure_ascii=True) + "\n"
        old_rendered = path.read_text()
        would_change = rendered != old_rendered
        if not would_change:
            continue

        changed.append(path)
        if args.check or args.dry_run:
            continue

        path.write_text(rendered)

    if args.check and changed:
        for path in changed:
            print(f"would anonymize {path}", file=sys.stderr)
        return 1

    if args.dry_run:
        for path in changed:
            print(f"would anonymize {path}")
    else:
        for path in changed:
            print(f"anonymized {path}")

    return 0


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="anonymize cached BGA replay fixtures")
    parser.add_argument(
        "paths",
        nargs="*",
        type=Path,
        default=[DEFAULT_FIXTURE_DIR],
        help="fixture JSON files or directories; defaults to tests/fixtures/bga_games",
    )
    parser.add_argument(
        "--review-user",
        help="real BGA username to preserve as the anonymized Reviewer",
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="reanonymize fixtures even if they already contain the anonymization marker",
    )
    parser.add_argument(
        "--check",
        action="store_true",
        help="fail if any selected fixture would change after anonymizing",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="show what would change without writing files",
    )
    return parser.parse_args()


def fixture_files(paths: list[Path]) -> list[Path]:
    files: list[Path] = []
    for path in paths:
        if path.is_dir():
            files.extend(sorted(path.glob("*.json")))
        elif path.is_file():
            files.append(path)
        else:
            raise SystemExit(f"fixture path does not exist: {path}")
    return sorted(dict.fromkeys(files))


def load_json(path: Path) -> dict[str, Any]:
    value = json.loads(path.read_text())
    if not isinstance(value, dict):
        raise SystemExit(f"{path}: fixture root must be a JSON object")
    return value


def fixture_table_id(path: Path, doc: dict[str, Any]) -> int:
    table_id = doc.get("table_id")
    if not isinstance(table_id, int):
        raise SystemExit(f"{path}: fixture is missing integer table_id")
    if path.name != f"{table_id}.json":
        raise SystemExit(
            f"{path}: fixture filename must match table_id {table_id}")
    return table_id


def replacement_plan(
    doc: dict[str, Any],
    table_id: int,
    review_user: str | None,
    preserve_labels: bool,
) -> ReplacementPlan:
    old_table_id = doc.get("table_id")
    if not isinstance(old_table_id, int):
        raise SystemExit("fixture is missing integer table_id")
    if old_table_id != table_id:
        raise SystemExit(
            f"fixture table_id changed during planning: {old_table_id} != {table_id}")

    players = players_from(doc)
    review_index = review_player_index(players, review_user)
    player_labels = labels_for(players, review_index, preserve_labels)

    int_values: dict[int, int] = {}
    str_values: dict[str, str] = {}
    substrings: dict[str, str] = {}

    for index, player in enumerate(players):
        raw_id = player.get("id")
        if not isinstance(raw_id, int):
            raise SystemExit(f"player {index + 1} is missing integer id")

        fake_id = PLAYER_ID_BASE + index
        raw_name = player.get("name")
        if not isinstance(raw_name, str) or not raw_name:
            raise SystemExit(f"player {index + 1} is missing name")

        int_values[raw_id] = fake_id
        str_values[str(raw_id)] = str(fake_id)
        str_values[raw_name] = player_labels[index]
        substrings[raw_name] = player_labels[index]
        substrings[f"/player/p{raw_id}"] = f"/player/p{fake_id}"

        raw_avatar = player.get("avatar")
        if isinstance(raw_avatar, str) and raw_avatar:
            str_values[raw_avatar] = f"anonymous_avatar_{index + 1}"
            substrings[raw_avatar] = f"anonymous_avatar_{index + 1}"

    return ReplacementPlan(
        table_id=table_id,
        int_values=int_values,
        str_values=str_values,
        substrings=substrings,
    )


def players_from(doc: dict[str, Any]) -> list[dict[str, Any]]:
    try:
        players = doc["logs"]["data"]["players"]
    except KeyError as exc:
        raise SystemExit("fixture is missing logs.data.players") from exc
    if not isinstance(players, list) or not players:
        raise SystemExit("logs.data.players must be a nonempty array")
    for player in players:
        if not isinstance(player, dict):
            raise SystemExit("logs.data.players entries must be objects")
    return players


def review_player_index(
    players: list[dict[str, Any]],
    review_user: str | None,
) -> int | None:
    if review_user is None:
        return None
    matches = [
        index
        for index, player in enumerate(players)
        if player.get("name") == review_user
    ]
    if len(matches) != 1:
        raise SystemExit(
            f"--review-user {review_user!r} matched {len(matches)} players")
    return matches[0]


def labels_for(
    players: list[dict[str, Any]],
    review_index: int | None,
    preserve_labels: bool,
) -> list[str]:
    if preserve_labels:
        labels = []
        for index, player in enumerate(players):
            name = player.get("name")
            if not isinstance(name, str) or not name:
                raise SystemExit(f"player {index + 1} is missing name")
            if not ANON_PLAYER_LABEL_RE.fullmatch(name):
                raise SystemExit(
                    f"marked fixture has non-anonymized player name {name!r}; "
                    "rerun with --review-user")
            labels.append(name)
        return labels

    if review_index is None:
        return [f"Player {index + 1}" for index in range(len(players))]

    labels = []
    opponent = 1
    for index, _player in enumerate(players):
        if index == review_index:
            labels.append("Reviewer")
        else:
            labels.append(f"Opponent {opponent}")
            opponent += 1
    return labels


def rewrite(value: Any, plan: ReplacementPlan) -> Any:
    if isinstance(value, dict):
        rewritten = {}
        for key, item in value.items():
            new_key = plan.str_values.get(key, key)
            if key == "country" and is_country_object(item):
                rewritten[new_key] = anonymous_country()
            elif key == "thumbs" and isinstance(item, dict):
                rewritten[new_key] = {}
            elif key == "avatar" and isinstance(item, str):
                rewritten[new_key] = anonymous_avatar(rewrite(item, plan))
            else:
                rewritten[new_key] = rewrite(item, plan)
        return rewritten

    if isinstance(value, list):
        return [rewrite(item, plan) for item in value]

    if isinstance(value, int) and value in plan.int_values:
        return plan.int_values[value]

    if isinstance(value, str):
        if value in plan.str_values:
            return plan.str_values[value]
        rewritten = value
        for old, new in sorted(plan.substrings.items(), key=lambda item: -len(item[0])):
            rewritten = rewritten.replace(old, new)
        return scrub_bga_channels(rewritten, plan)

    return value


def is_country_object(value: Any) -> bool:
    return (
        isinstance(value, dict)
        and "code" in value
        and "name" in value
        and "flag_x" in value
        and "flag_y" in value
    )


def anonymous_country() -> dict[str, Any]:
    return {
        "code": "ZZ",
        "name": "Anonymous",
        "cur": "ZZ",
        "flag_x": 0,
        "flag_y": 0,
    }


def anonymous_avatar(value: Any) -> str:
    if isinstance(value, str) and value.startswith("anonymous_avatar"):
        return value
    return "anonymous_avatar"


def scrub_bga_channels(value: str, plan: ReplacementPlan) -> str:
    value = BGA_TABLE_CHANNEL_RE.sub(lambda _match: f"/table/t{plan.table_id}", value)

    def scrub_player(match: re.Match[str]) -> str:
        raw_id = int(match.group(1))
        fake_id = plan.int_values.get(raw_id, UNKNOWN_PLAYER_CHANNEL_ID)
        return f"/player/p{fake_id}"

    return BGA_PLAYER_CHANNEL_RE.sub(scrub_player, value)


if __name__ == "__main__":
    raise SystemExit(main())
