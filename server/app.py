#!/usr/bin/env python3

"""
Local web server for the RftG game review UI.

Stdlib only.  Serves the static UI plus a small JSON API:

  GET  /api/games                     list available analyses
  GET  /api/analysis/<id>             one game's analysis (JSON array)
  GET  /api/bga/players/<table_id>    cached BGA table players
  POST /api/demo                      generate+analyze a demo AI game
  POST /api/bga/sync                  refresh BGA game list (cookie req)
  POST /api/bga/analyze/<table_id>    fetch, parse and analyze a perspective

Run:  python3 server/app.py  [port]
"""

import json
import subprocess
import sys
import urllib.parse
from http.server import HTTPServer, BaseHTTPRequestHandler
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
UI_DIR = ROOT / "ui"
ENGINE = ROOT / "engine"
DATA = ROOT / "data"
ANALYSIS_DIR = DATA / "analysis"
SCRIPTS_DIR = DATA / "scripts"
BGA_GAMES_DIR = DATA / "games"

sys.path.insert(0, str(ROOT / "server"))


def run_analyzer(script_path: Path, out_path: Path) -> dict:
    """Run the analyzer on a script, store JSONL, return summary."""
    proc = subprocess.run(
        [str(ENGINE / "analyzer"), str(script_path)],
        capture_output=True, text=True, timeout=600)
    if proc.returncode != 0:
        raise RuntimeError(
            f"analyzer failed (rc={proc.returncode}): "
            f"{proc.stderr.strip()[-500:]}")
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(proc.stdout)
    events = [json.loads(l) for l in proc.stdout.splitlines() if l.strip()]
    decisions = [e for e in events if e.get("event") == "decision"]
    result = next((e for e in events if e.get("event") == "result"), None)
    return {"decisions": len(decisions), "result": result}


_card_text = None


def card_text() -> dict:
    """Card name -> readable description (for UI tooltips), rendered
    from the cards.txt definition blocks."""
    global _card_text
    if _card_text is None:
        import cardtext
        cards = {}
        name = None
        lines = []
        for raw in (ENGINE / "data" / "cards.txt").read_text().splitlines():
            if raw.startswith("N:"):
                if name:
                    cards[name] = cardtext.card_to_text(lines)
                name = raw[2:].strip()
                lines = []
            elif name and raw.strip() and not raw.startswith(("E@", "#")):
                lines.append(raw.strip())
        if name:
            cards[name] = cardtext.card_to_text(lines)
        _card_text = cards
    return _card_text


def list_games() -> list:
    games = []
    if ANALYSIS_DIR.is_dir():
        for f in sorted(ANALYSIS_DIR.glob("*.jsonl")):
            meta_path = f.with_suffix(".meta.json")
            meta = (json.loads(meta_path.read_text())
                    if meta_path.exists() else {})
            games.append({"id": f.stem, **meta})
    return games


def bga_analysis_id(table_id: int, player_id: int) -> str:
    return f"bga-{table_id}-p{player_id}"


def bga_players(raw: dict) -> list:
    return [
        {"id": int(p["id"]), "name": p["name"], "seat": i}
        for i, p in enumerate(raw["logs"]["data"]["players"])
    ]


def cached_bga_table(table_id: int) -> dict:
    f = BGA_GAMES_DIR / f"{table_id}.json"
    if not f.exists():
        raise FileNotFoundError(
            f"cached BGA table {table_id} not found; analyze it first")
    return json.loads(f.read_text())


def load_bga_table(table_id: int, force: bool = False) -> dict:
    f = BGA_GAMES_DIR / f"{table_id}.json"
    if f.exists() and not force:
        return json.loads(f.read_text())
    import bga
    return bga.client_from_config().fetch_table(table_id, force=force)


def configured_bga_username() -> str:
    import bga
    username = str(bga.load_config().get("bga_username", "")).strip()
    if not username or username == "your-bga-username":
        raise RuntimeError(
            "set bga_username in config.json to one of the table players")
    return username


def select_bga_player(raw: dict, params: dict) -> dict:
    players = bga_players(raw)
    if "player_id" in params:
        try:
            player_id = int(params["player_id"])
        except ValueError as exc:
            raise ValueError(f"invalid player_id {params['player_id']!r}") from exc
        player = next((p for p in players if p["id"] == player_id), None)
        if player is None:
            names = ", ".join(f"{p['name']} ({p['id']})" for p in players)
            raise ValueError(
                f"player_id {player_id} is not in table; players: {names}")
        return player

    if "player" in params:
        name = params["player"]
    else:
        name = configured_bga_username()

    player = next((p for p in players if p["name"] == name), None)
    if player is None:
        names = ", ".join(p["name"] for p in players)
        raise ValueError(f"player {name!r} is not in table; players: {names}")
    return player


def analyze_bga_table(table_id: int, params: dict) -> dict:
    import haskell_parser

    force = params.get("force") == "1"
    raw = load_bga_table(table_id, force=force)
    player = select_bga_player(raw, params)
    game_id = bga_analysis_id(table_id, player["id"])
    game_path = BGA_GAMES_DIR / f"{table_id}.json"
    script_path = SCRIPTS_DIR / f"{game_id}.script"
    out_path = ANALYSIS_DIR / f"{game_id}.jsonl"
    meta_path = ANALYSIS_DIR / f"{game_id}.meta.json"

    if not force and out_path.exists() and meta_path.exists():
        meta = json.loads(meta_path.read_text())
        return {"id": game_id, "cached": True, **meta}

    script = haskell_parser.parse_file(
        game_path, player["name"], player_id=player["id"])
    SCRIPTS_DIR.mkdir(parents=True, exist_ok=True)
    script_path.write_text(script)
    summary = run_analyzer(script_path, out_path)
    meta = {
        "source": "bga",
        "table_id": table_id,
        "players": bga_players(raw),
        "perspective": {
            "player_id": player["id"],
            "name": player["name"],
            "seat": player["seat"],
        },
        **summary,
    }
    meta_path.write_text(json.dumps(meta))
    return {"id": game_id, **meta}


def generate_demo(params: dict) -> dict:
    players = int(params.get("players", 2))
    expansion = int(params.get("expansion", 3))
    seed = int(params.get("seed", 1))
    advanced = int(params.get("advanced", 0))

    SCRIPTS_DIR.mkdir(parents=True, exist_ok=True)
    game_id = f"demo-p{players}-e{expansion}{'-a' if advanced else ''}-{seed}"
    script_path = SCRIPTS_DIR / f"{game_id}.script"

    cmd = [str(ENGINE / "genscript"), "-p", str(players),
           "-e", str(expansion), "-r", str(seed)]
    if advanced:
        cmd.append("-a")
    proc = subprocess.run(cmd, capture_output=True, text=True, timeout=300)
    if proc.returncode != 0:
        raise RuntimeError(f"genscript failed: {proc.stderr[-300:]}")
    script_path.write_text(proc.stdout)

    out_path = ANALYSIS_DIR / f"{game_id}.jsonl"
    summary = run_analyzer(script_path, out_path)

    meta = {
        "source": "demo",
        "players": players,
        "expansion": expansion,
        "advanced": advanced,
        "seed": seed,
        **summary,
    }
    (ANALYSIS_DIR / f"{game_id}.meta.json").write_text(json.dumps(meta))
    return {"id": game_id, **meta}


class Handler(BaseHTTPRequestHandler):
    def log_message(self, fmt, *args):
        sys.stderr.write("%s - %s\n" % (self.address_string(), fmt % args))

    def _send(self, code: int, body: bytes, ctype: str):
        self.send_response(code)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def _json(self, obj, code=200):
        self._send(code, json.dumps(obj).encode(), "application/json")

    def _error(self, msg, code=500):
        self._json({"error": str(msg)}, code)

    def do_GET(self):
        url = urllib.parse.urlparse(self.path)
        path = url.path

        if path == "/api/games":
            return self._json(list_games())

        if path == "/api/cards":
            return self._json(card_text())

        if path.startswith("/api/analysis/"):
            game_id = path.split("/")[-1]
            f = ANALYSIS_DIR / f"{game_id}.jsonl"
            if not f.exists():
                return self._error("not found", 404)
            events = [json.loads(l) for l in f.read_text().splitlines()
                      if l.strip()]
            return self._json(events)

        if path.startswith("/api/bga/players/"):
            try:
                table_id = int(path.split("/")[-1])
                raw = cached_bga_table(table_id)
                return self._json({
                    "table_id": table_id,
                    "players": bga_players(raw),
                })
            except Exception as exc:
                return self._error(f"{type(exc).__name__}: {exc}")

        # Static files
        if path == "/":
            path = "/index.html"
        f = (UI_DIR / path.lstrip("/")).resolve()
        if UI_DIR.resolve() in f.parents and f.is_file():
            ctype = {
                ".html": "text/html", ".js": "text/javascript",
                ".css": "text/css", ".png": "image/png",
                ".svg": "image/svg+xml",
            }.get(f.suffix, "application/octet-stream")
            return self._send(200, f.read_bytes(), ctype)

        self._error("not found", 404)

    def do_POST(self):
        url = urllib.parse.urlparse(self.path)
        params = dict(urllib.parse.parse_qsl(url.query))
        path = url.path

        try:
            if path == "/api/demo":
                return self._json(generate_demo(params))

            if path.startswith("/api/delete/"):
                game_id = path.split("/")[-1]
                # Only allow deleting known analysis artifacts
                if not game_id or "/" in game_id or ".." in game_id:
                    return self._error("bad id", 400)
                removed = 0
                for f in (ANALYSIS_DIR / f"{game_id}.jsonl",
                          ANALYSIS_DIR / f"{game_id}.meta.json",
                          SCRIPTS_DIR / f"{game_id}.script"):
                    if f.exists():
                        f.unlink()
                        removed += 1
                return self._json({"deleted": game_id,
                                   "files": removed})

            if path == "/api/bga/sync":
                import bga
                client = bga.client_from_config()
                tables = client.list_games(
                    page=int(params.get("page", 1)))
                (DATA / "bga_games.json").write_text(json.dumps(tables))
                return self._json({"tables": tables})

            if path.startswith("/api/bga/analyze/"):
                table_id = int(path.split("/")[-1])
                return self._json(analyze_bga_table(table_id, params))

            self._error("not found", 404)
        except Exception as exc:  # surface to the UI
            self._error(f"{type(exc).__name__}: {exc}")


def main():
    port = int(sys.argv[1]) if len(sys.argv) > 1 else 8421
    ANALYSIS_DIR.mkdir(parents=True, exist_ok=True)
    server = HTTPServer(("127.0.0.1", port), Handler)
    print(f"rftg-analyzer ui: http://127.0.0.1:{port}/")
    server.serve_forever()


if __name__ == "__main__":
    main()
