"""
Debug tool: print a readable, deduplicated timeline of a cached BGA
RftG game.

    python3 server/timeline.py data/games/<table>.json [gamedatas.json]
"""

import json
import sys
from pathlib import Path

SKIP = {
    "updateReflexionTime", "updateScore", "updateCardCount",
    "simpleNote", "simpleNode", "updateSixCostDevelopmentVp",
    "wakeupPlayers", "clearTmpMilforce", "prestige_search",
    "updateSpecializedMilitary", "gameStateMultipleActiveUpdate",
    "updateProduceTitle", "setPlayerCounter", "updateMilforce",
    "yourturn", "drawCards_log", "explored_choice_log",
    "keepcards_log",
}


def load(table_path, gamedatas_path=None):
    raw = json.loads(Path(table_path).read_text())
    gd = (json.loads(Path(gamedatas_path).read_text())
          if gamedatas_path else raw.get("gamedatas"))
    return raw, gd


def build_maps(raw, gd):
    """Return (type_names, instance_types) maps."""
    type_names = {}
    if gd:
        type_names = {int(k): v["name"]
                      for k, v in gd.get("card_types", {}).items()}
    inst = {}

    def see(card):
        if isinstance(card, dict) and "id" in card and "type" in card:
            inst[int(card["id"])] = int(card["type"])

    if gd:
        for zone in ("hand", "tableau", "explored", "good"):
            z = gd.get(zone)
            if isinstance(z, dict):
                for c in z.values():
                    see(c)

    for pkt in raw["logs"]["data"]["logs"]:
        for n in pkt.get("data", []):
            a = n.get("args")
            if isinstance(a, list):
                for c in a:
                    see(c)
            elif isinstance(a, dict):
                see(a.get("card"))
                for v in a.values():
                    see(v)
                cards = a.get("cards")
                if isinstance(cards, dict):
                    for c in cards.values():
                        see(c)
    return type_names, inst


def iter_dedup(raw):
    """Yield (move_id, notif) deduplicated by uid."""
    seen = set()
    for pkt in raw["logs"]["data"]["logs"]:
        mid = pkt.get("move_id")
        for n in pkt.get("data", []):
            uid = n.get("uid")
            if uid and uid in seen:
                continue
            if uid:
                seen.add(uid)
            yield mid, n


def main():
    raw, gd = load(sys.argv[1], sys.argv[2] if len(sys.argv) > 2 else None)
    type_names, inst = build_maps(raw, gd)
    players = {int(p["id"]): p["name"]
               for p in raw["logs"]["data"]["players"]}

    def iname(card_id):
        t = inst.get(int(card_id))
        return type_names.get(t, f"inst{card_id}")

    def cname(c):
        return type_names.get(int(c["type"]), f"type{c['type']}")

    def pname(pid):
        return players.get(int(pid), str(pid))

    # Map good instance -> world instance (goods are anonymous cards)
    good_world = {}
    for _, n in iter_dedup(raw):
        if n["type"] == "goodproduction":
            a = n["args"]
            good_world[int(a["good_id"])] = int(a["world_id"])

    for mid, n in iter_dedup(raw):
        t, a = n["type"], n.get("args")
        if t in SKIP:
            continue
        line = None
        if t == "gameStateChange":
            nm = a.get("name") if isinstance(a, dict) else None
            if nm and nm != "gameSetup":
                line = f"STATE {nm}"
        elif t == "drawCards":
            line = (f"drawCards {pname(a[0]['location_arg'])}: " +
                    ", ".join(cname(c) for c in a))
        elif t == "drawCards_def":
            line = f"drawCards_def {a['player_name']} x{a['card_nbr']}"
        elif t == "explored_choice":
            line = (f"explored {pname(a[0]['location_arg'])}: " +
                    ", ".join(cname(c) for c in a))
        elif t == "keepcards":
            cards = list(a.values()) if isinstance(a, dict) else a
            who = pname(cards[0]["location_arg"]) if cards else "?"
            line = (f"keepcards {who}: " +
                    ", ".join(cname(c) for c in cards))
        elif t == "playcard":
            line = (f"playcard {pname(a['player'])}: "
                    f"{cname(a['card'])}")
        elif t == "cardcost":
            line = (f"cardcost {cname(a['card'])} cost={a['cost']} "
                    f"mil={a.get('military_force')} "
                    f"cs={a.get('use_contact_specialist')}")
        elif t == "discard":
            line = ("discard: " +
                    ", ".join(iname(c) for c in a["cards"]))
        elif t == "discardfromtableau":
            line = f"discardfromtableau: {iname(a['card'])}"
        elif t == "phase_choices":
            picks = {ph: v for ph, v in a.items() if v}
            line = f"phase_choices {json.dumps(picks)}"
        elif t == "goodproduction":
            line = (f"good on {iname(a['world_id'])} "
                    f"(good_id={a['good_id']})")
        elif t == "consume":
            gid = int(a["good_id"])
            world = good_world.get(gid)
            line = (f"consume {pname(a['player_id'])} good from "
                    f"{iname(world) if world else f'good{gid}'}")
        elif t == "showTableau":
            cards = a.get("cards", {})
            byp = {}
            for c in cards.values():
                byp.setdefault(pname(c["location_arg"]), []).append(
                    cname(c))
            line = "showTableau " + json.dumps(byp)
        elif t == "reshuffle":
            line = "reshuffle"
        else:
            line = f"{t} {json.dumps(a)[:160]}"
        if line:
            print(f"m{mid}\t{line}")


if __name__ == "__main__":
    main()
