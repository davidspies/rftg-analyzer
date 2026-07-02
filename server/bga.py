"""
BoardGameArena fetcher for Race for the Galaxy replays.

Uses a session cookie pasted by the user into config.json.  Downloads
the user's finished RftG games and per-table replay logs, caching raw
JSON under data/.

BGA has no official API; these endpoints are the ones the website
itself uses.  If BGA changes them, this module is the only place to
update.
"""

import json
import re
import time
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path

BASE = "https://boardgamearena.com"

# BGA's internal game id for Race for the Galaxy
RFTG_GAME_ID = 10

DATA_DIR = Path(__file__).resolve().parent.parent / "data"
GAMES_DIR = DATA_DIR / "games"


class BGAError(Exception):
    pass


def _balanced_json(html: str, j: int) -> dict:
    """Parse a JSON object starting at html[j] (a '{')."""
    depth = 0
    k = j
    in_str = False
    esc = False
    while True:
        c = html[k]
        if in_str:
            if esc:
                esc = False
            elif c == "\\":
                esc = True
            elif c == '"':
                in_str = False
        else:
            if c == '"':
                in_str = True
            elif c == "{":
                depth += 1
            elif c == "}":
                depth -= 1
                if depth == 0:
                    break
        k += 1
    return json.loads(html[j:k + 1])


def extract_gamelogs(html: str) -> dict:
    """Extract the per-perspective notification log embedded in a
    replay page (g_gamelogs).  These contain private draws that the
    global archive log redacts."""
    i = html.find("g_gamelogs = ")
    if i < 0:
        raise BGAError("no g_gamelogs in replay page")
    j = html.find("{", i)
    return _balanced_json(html, j)


def extract_gamedatas(html: str) -> dict:
    """Extract the gamedatas JSON embedded in a replay page (passed as
    an argument to gameui.completesetup)."""
    i = html.find("completesetup(")
    if i < 0:
        raise BGAError("no completesetup call in replay page")
    j = html.find("{", i)
    depth = 0
    k = j
    in_str = False
    esc = False
    while True:
        c = html[k]
        if in_str:
            if esc:
                esc = False
            elif c == "\\":
                esc = True
            elif c == '"':
                in_str = False
        else:
            if c == '"':
                in_str = True
            elif c == "{":
                depth += 1
            elif c == "}":
                depth -= 1
                if depth == 0:
                    break
        k += 1
    return json.loads(html[j:k + 1])


class BGAClient:
    def __init__(self, cookie: str, player_id: int | None = None):
        self.cookie = cookie.strip()
        self.player_id = player_id
        self.request_token = None
        GAMES_DIR.mkdir(parents=True, exist_ok=True)

    def _get(self, path: str, params: dict | None = None,
             referer: str | None = None, _retry: bool = True) -> str:
        url = BASE + path
        if params:
            url += "?" + urllib.parse.urlencode(params)
        req = urllib.request.Request(url)
        req.add_header("Cookie", self.cookie)
        req.add_header("User-Agent",
                       "Mozilla/5.0 (X11; Linux x86_64) rftg-analyzer")
        req.add_header("X-Requested-With", "XMLHttpRequest")
        if self.request_token:
            req.add_header("X-Request-Token", self.request_token)
        if referer:
            req.add_header("Referer", referer)
        try:
            with urllib.request.urlopen(req, timeout=30) as resp:
                return resp.read().decode("utf-8", errors="replace")
        except urllib.error.HTTPError as exc:
            # BGA occasionally hiccups or invalidates the request
            # token; refresh and retry once
            if _retry and exc.code >= 500:
                time.sleep(2)
                self.request_token = None
                if path != "/":
                    self.whoami()
                return self._get(path, params, referer, _retry=False)
            raise BGAError(
                f"BGA request to {path} failed: {exc} (if this "
                f"persists your session cookie may have expired -- "
                f"re-paste it into config.json)") from exc

    def _get_json(self, path: str, params: dict | None = None) -> dict:
        text = self._get(path, params)
        try:
            data = json.loads(text)
        except json.JSONDecodeError:
            raise BGAError(
                f"non-JSON response from {path} (cookie expired or "
                f"blocked?): {text[:200]}")
        if isinstance(data, dict) and data.get("status") == "0":
            raise BGAError(f"BGA error from {path}: "
                           f"{data.get('error', data)}")
        return data

    def whoami(self) -> dict:
        """Fetch current player info; also picks up a request token."""
        # The main page embeds the player id and request token
        html = self._get("/")
        m = re.search(r"requestToken[^A-Za-z0-9]+([A-Za-z0-9]+)", html)
        if m:
            self.request_token = m.group(1)
        if not self.player_id:
            m = re.search(r'"user_infos":\{"id":"?(\d+)', html)
            if m:
                self.player_id = int(m.group(1))
        return {"player_id": self.player_id,
                "token": bool(self.request_token)}

    def list_games(self, page: int = 1, finished: bool = True) -> list:
        """List one page (10 tables) of the user's RftG games, most
        recent first."""
        if not self.player_id:
            self.whoami()
        if not self.player_id:
            raise BGAError("could not determine player id; set "
                           "player_id in config.json")
        data = self._get_json("/gamestats/gamestats/getGames.html", {
            "player": self.player_id,
            "opponent_id": 0,
            "game_id": RFTG_GAME_ID,
            "finished": 1 if finished else 0,
            "page": page,
            "updateStats": 0,
        })
        return data.get("data", {}).get("tables", [])

    def fetch_table(self, table_id: int, force: bool = False) -> dict:
        """Fetch and cache all raw data needed to analyze one table."""
        out = GAMES_DIR / f"{table_id}.json"
        if out.exists() and not force:
            return json.loads(out.read_text())

        result = {"table_id": table_id, "fetched_at": time.time()}

        # Ensure we hold a request token
        if not self.request_token:
            self.whoami()

        # Table metadata (players, options, result)
        result["tableinfos"] = self._get_json(
            "/table/table/tableinfos.html", {"id": table_id})

        # Archived tables must be extracted server-side before their
        # notification log can be read
        try:
            self._get_json(
                "/gamereview/gamereview/requestTableArchive.html",
                {"table": table_id})
            time.sleep(1)
        except BGAError:
            pass  # recent tables need no extraction

        # Full game log (notification stream)
        result["logs"] = self._get_json(
            "/archive/archive/logs.html",
            {"table": table_id, "translated": "true"})

        # Per-player initial state (gamedatas embedded in the replay
        # page): gives each player's starting hand, the card-type
        # database, goal list, etc.
        version = self._replay_version(table_id)
        result["gamedatas"] = {}
        result["gamelogs"] = {}
        for p in result["logs"]["data"]["players"]:
            pid = int(p["id"])
            html = self._get(f"/archive/replay/{version}/",
                             {"table": table_id, "player": pid,
                              "comments": pid})
            result["gamedatas"][str(pid)] = extract_gamedatas(html)
            result["gamelogs"][str(pid)] = extract_gamelogs(html)

        out.write_text(json.dumps(result, indent=1))
        return result

    def _replay_version(self, table_id: int) -> str:
        """Discover the archive replay version path for a table."""
        html = self._get("/gamereview", {"table": table_id})
        m = re.search(r"archive/replay/([0-9-]+)/", html)
        if not m:
            raise BGAError("cannot find replay link on gamereview page")
        return m.group(1)


def load_config() -> dict:
    cfg_path = Path(__file__).resolve().parent.parent / "config.json"
    if not cfg_path.exists():
        raise BGAError(
            f"create {cfg_path} from config.example.json and paste "
            f"your BGA cookie into it")
    return json.loads(cfg_path.read_text())


def client_from_config() -> BGAClient:
    cfg = load_config()
    return BGAClient(cfg["bga_cookie"], cfg.get("player_id"))


if __name__ == "__main__":
    import sys

    client = client_from_config()
    if len(sys.argv) > 1:
        table = int(sys.argv[1])
        data = client.fetch_table(table, force="-f" in sys.argv)
        print(f"fetched table {table}: "
              f"{len(json.dumps(data))} bytes cached")
    else:
        games = client.list_games()
        for g in games:
            print(g.get("table_id", g))
