import subprocess
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
PARSER_HS = ROOT / "parser-hs"

_parser = None


def executable() -> Path:
    """Build and return the Haskell BGA parser executable."""
    global _parser
    if _parser and _parser.exists():
        return _parser

    _run_cabal(["build", "exe:rftg-bga-parser"], timeout=300)
    listed = _run_cabal(["list-bin", "exe:rftg-bga-parser"], timeout=60)

    parser = Path(listed.stdout.strip())
    if not parser.exists():
        raise RuntimeError(f"Haskell parser executable is missing: {parser}")
    _parser = parser
    return parser


def parse_file(game_path: Path, username: str = "", player_id: int | None = None) -> str:
    cmd = [str(executable()), str(game_path)]
    if player_id is not None:
        cmd += ["--player-id", str(player_id)]
    elif username:
        cmd += ["--user", username]
    try:
        proc = subprocess.run(
            cmd,
            cwd=ROOT,
            capture_output=True,
            text=True,
            timeout=300,
        )
    except FileNotFoundError as exc:
        raise RuntimeError(f"Haskell parser executable is missing: {exc}") from exc
    if proc.returncode != 0:
        raise RuntimeError(
            "Haskell parser failed: "
            f"{_tail(proc.stderr)}")
    return proc.stdout


def _run_cabal(args: list[str], timeout: int) -> subprocess.CompletedProcess:
    try:
        proc = subprocess.run(
            ["cabal", *args],
            cwd=PARSER_HS,
            capture_output=True,
            text=True,
            timeout=timeout,
        )
    except FileNotFoundError as exc:
        raise RuntimeError("cabal is missing from PATH") from exc
    if proc.returncode != 0:
        raise RuntimeError(
            f"cabal {' '.join(args)} failed: "
            f"{_tail(proc.stderr or proc.stdout)}")
    return proc


def _tail(text: str) -> str:
    return text.strip()[-500:]
