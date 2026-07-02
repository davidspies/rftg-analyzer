"""
Translate cards.txt card definitions into human-readable text for UI
tooltips.  Phrasing follows the rulebook loosely; anything unmapped
falls back to the raw power code so nothing is silently dropped.
"""

GOODS = {"NOVELTY": "Novelty", "RARE": "Rare", "GENE": "Genes",
         "ALIEN": "Alien"}

PHASES = {1: "I (Explore)", 2: "II (Develop)", 3: "III (Settle)",
          4: "IV (Consume)", 5: "V (Produce)"}

FLAG_TEXT = {
    "MILITARY": None,            # shown via the type line
    "WINDFALL": None,            # shown via the good line
    "START": None, "START_RED": None, "START_BLUE": None,
    "PROMO": None,
    "REBEL": "Rebel", "IMPERIUM": "Imperium", "ALIEN": "Alien",
    "UPLIFT": "Uplift", "CHROMO": "Chromosome",
    "TERRAFORMING": "Terraforming", "PRESTIGE": "Prestige",
    "XENO": "Xeno", "ANTI_XENO": "Anti-Xeno",
    "TAKE_DISCARDS": "takes all other players' end-of-round discards",
    "SELECT_LAST": "select actions after seeing others' choices",
    "EXTRA_SURVEY": "extra survey", "DISCARD_TO_12": "hand limit 12",
    "GAME_END_14": "game ends at 14 cards",
    "START_SAVE": None, "STARTHAND_3": "start with 3 cards",
    "NO_PRODUCE": "never produces", "DISCARD_PRODUCE": None,
}


def _n(v, word):
    if word in ("VP", "prestige"):
        return f"{v} {word}"
    return f"{v} {word}{'s' if int(v) != 1 else ''}"


def _consume(codes, v, t):
    """Phase 4 consume-power phrasing."""
    qty = 2 if "CONSUME_TWO" in codes else 1

    if "DISCARD_HAND" in codes:
        what = _n(qty, "card") + " from hand"
    elif "CONSUME_THIS" in codes:
        what = "the good on this world"
    elif "CONSUME_ALL" in codes:
        what = "all your goods"
    elif "CONSUME_3_DIFF" in codes:
        what = "3 goods of different kinds"
    elif "CONSUME_N_DIFF" in codes:
        what = "goods of different kinds"
    elif "CONSUME_PRESTIGE" in codes:
        what = "1 prestige"
    else:
        kinds = [GOODS[c[8:]] for c in codes
                 if c.startswith("CONSUME_") and c[8:] in GOODS]
        if kinds:
            base = "/".join(kinds)
            what = f"{qty} {base} goods" if qty > 1 else f"a {base} good"
        elif "CONSUME_ANY" in codes:
            what = f"{qty} goods" if qty > 1 else "any good"
        else:
            return None

    rewards = []
    if "GET_VP" in codes:
        rewards.append(_n(v, "VP"))
    if "GET_CARD" in codes:
        rewards.append("1 card")
    if "GET_2_CARD" in codes:
        rewards.append("2 cards")
    if "GET_3_CARD" in codes:
        rewards.append("3 cards")
    if "GET_PRESTIGE" in codes:
        rewards.append("1 prestige")
    if not rewards:
        return None

    txt = f"consume {what} for {' + '.join(rewards)}"
    if "CONSUME_ALL" in codes:
        txt = f"consume all your goods for {_n(v, 'VP')} each, less one"
    if "CONSUME_N_DIFF" in codes:
        txt += " each"
    if t > 1:
        txt += f" (up to {t}×)"
    return txt


def _military_qualifier(codes):
    """Qualifier for EXTRA_MILITARY-style powers."""
    quals = []
    for c in codes:
        if c in GOODS:
            quals.append(f"against {GOODS[c]} worlds")
        elif c == "AGAINST_REBEL":
            quals.append("against Rebel worlds")
        elif c == "AGAINST_CHROMO":
            quals.append("against Chromosome worlds")
        elif c == "XENO":
            quals.append("against Xeno")
        elif c == "PER_MILITARY":
            quals.append("per Military world")
        elif c == "PER_CHROMO":
            quals.append("per Chromosome card")
        elif c == "PER_IMPERIUM":
            quals.append("per Imperium card")
        elif c == "PER_REBEL_MILITARY":
            quals.append("per Rebel Military world")
        elif c == "PER_PEACEFUL":
            quals.append("per peaceful world")
        elif c == "IF_IMPERIUM":
            quals.append("if you have an Imperium card")
    return (" " + ", ".join(quals)) if quals else ""


def power_text(phase, codes_s, v, t):
    codes = set(c.strip() for c in codes_s.split("|"))
    v, t = int(v), int(t)
    handled = set()
    parts = []

    def take(*cs):
        handled.update(cs)

    # cards.txt reuses the mnemonic "DISCARD" for two distinct engine
    # flags, told apart by the phase namespace Keldon's own parser uses:
    #   P3_DISCARD -> discards THIS card from the tableau (one-shot power)
    #   P5_DISCARD -> discards a card from HAND as a cost to produce
    prefix = ""
    prod_prefix = ""
    if "DISCARD" in codes and phase == 3:
        prefix = "discard this card to "
        take("DISCARD")
    elif "DISCARD" in codes and phase == 5:
        prod_prefix = "may discard a card from hand to "
        take("DISCARD")

    if phase == 4:
        c_txt = _consume(codes, v, t)
        if c_txt:
            return prefix + c_txt

    if "DRAW" in codes:
        parts.append(f"draw {_n(v, 'card')}")
        take("DRAW")
    if "KEEP" in codes:
        parts.append(f"keep {v} extra")
        take("KEEP")
    if "DISCARD_ANY" in codes:
        parts.append("may discard any cards from hand, not just "
                     "those drawn")
        take("DISCARD_ANY")
    if "DRAW_AFTER" in codes:
        what = {2: "developing", 3: "placing a world"}.get(phase,
                                                          "this phase")
        parts.append(f"draw {_n(v, 'card')} after {what}")
        take("DRAW_AFTER")
    if "EXPLORE_AFTER" in codes:
        parts.append(f"explore +{v} after this phase")
        take("EXPLORE_AFTER")
    if "REDUCE" in codes:
        kind = next((GOODS[c] for c in codes if c in GOODS), None)
        tgt = {2: "developments", 3: "worlds"}.get(phase, "cards")
        if kind:
            tgt = f"{kind} {tgt}"
            take(kind.upper() if kind != "Genes" else "GENE")
        parts.append(f"-{v} cost to place {tgt}")
        take("REDUCE", *[c for c in codes if c in GOODS])
    if "REDUCE_ZERO" in codes:
        parts.append("reduce a (non-Alien) world's cost to 0")
        take("REDUCE_ZERO")
    if "EXTRA_MILITARY" in codes:
        sign = "+" if v >= 0 else ""
        parts.append(f"{sign}{v} Military" + _military_qualifier(codes))
        take("EXTRA_MILITARY", "AGAINST_REBEL", "AGAINST_CHROMO",
             "XENO", "PER_CHROMO", "PER_MILITARY", "PER_IMPERIUM",
             "PER_REBEL_MILITARY", "PER_PEACEFUL", "IF_IMPERIUM",
             *[c for c in codes if c in GOODS])
    if "MILITARY_HAND" in codes:
        parts.append(f"may discard up to {_n(v, 'card')} from hand "
                     "for +1 Military each")
        take("MILITARY_HAND")
    for kind, name in GOODS.items():
        cc = "CONSUME_" + kind
        if cc in codes and phase == 3:
            parts.append(f"may consume a {name} good for "
                         f"+{v} Military")
            take(cc)
    if "CONSUME_PRESTIGE" in codes and phase == 3:
        parts.append(f"may spend 1 prestige for +{v} Military")
        take("CONSUME_PRESTIGE")
    if "PAY_MILITARY" in codes:
        anti = ""
        if "AGAINST_REBEL" in codes:
            anti = "Rebel "
            take("AGAINST_REBEL")
        if "AGAINST_CHROMO" in codes:
            anti = "Chromosome "
            take("AGAINST_CHROMO")
        d = f" at -{v} cost" if v else ""
        parts.append(f"may pay for {anti}military worlds{d}")
        take("PAY_MILITARY")
    if "PAY_PRESTIGE" in codes:
        parts.append(f"+{_n(v, 'prestige')} when paying for a "
                     "military world")
        take("PAY_PRESTIGE")
    if "PAY_DISCOUNT" in codes:
        parts.append(f"-{v} cost when paying for a military world")
        take("PAY_DISCOUNT")
    if "CONQUER_SETTLE" in codes:
        parts.append(f"conquer a peaceful world (+{v} Military)")
        take("CONQUER_SETTLE")
    if "PLACE_TWO" in codes:
        parts.append("may place a second world")
        take("PLACE_TWO")
    if "PLACE_MILITARY" in codes:
        parts.append("may place an additional military world")
        take("PLACE_MILITARY")
    if "PLACE_LEFTOVER" in codes:
        parts.append("may place a world with leftover Explore cards")
        take("PLACE_LEFTOVER")
    if "PLACE_ZERO" in codes:
        parts.append("may place an additional world at cost 0")
        take("PLACE_ZERO")
    if "FLIP_ZERO" in codes:
        parts.append("may flip a card for a free world")
        take("FLIP_ZERO")
    if "UPGRADE_WORLD" in codes:
        parts.append(f"may replace a world with one costing up to "
                     f"{v} more")
        take("UPGRADE_WORLD")
    if "AUTO_PRODUCE" in codes:
        parts.append("produces its good when placed")
        take("AUTO_PRODUCE")
    if "SAVE_COST" in codes:
        parts.append("may save one payment card under this world")
        take("SAVE_COST")
    if "TAKE_SAVED" in codes:
        parts.append("take cards saved under this world into hand")
        take("TAKE_SAVED")
    if "XENO_DEFENSE" in codes:
        parts.append(f"+{v} defense against Xeno invasion")
        take("XENO_DEFENSE")
    if "REPAIR" in codes:
        parts.append("repair a damaged world")
        take("REPAIR")

    # Trade powers
    for kind, name in GOODS.items():
        tc = "TRADE_" + kind
        if tc in codes:
            parts.append(f"+{_n(v, 'card')} when selling a {name} good")
            take(tc)
    if "TRADE_ANY" in codes:
        parts.append(f"+{_n(v, 'card')} when selling any good")
        take("TRADE_ANY")
    if "TRADE_THIS" in codes:
        parts.append(f"+{_n(v, 'card')} when selling this world's good")
        take("TRADE_THIS")
    if "TRADE_BONUS_CHROMO" in codes:
        parts.append("+1 card per Chromosome card when selling")
        take("TRADE_BONUS_CHROMO")
    if "TRADE_ACTION" in codes:
        nb = " (no Trade bonuses)" if "TRADE_NO_BONUS" in codes else ""
        parts.append(f"may sell a good as a consume power{nb}")
        take("TRADE_ACTION", "TRADE_NO_BONUS")
    if "NO_TRADE" in codes:
        parts.append("good here may not be sold")
        take("NO_TRADE")
    if "VP" in codes:
        parts.append(f"+{_n(v, 'VP')} (no good needed)")
        take("VP")
    if "GET_PRESTIGE" in codes and phase != 4:
        parts.append("+1 prestige")
        take("GET_PRESTIGE")

    # Produce phase (prod_prefix carries a from-hand discard cost, if any)
    if "PRODUCE" in codes:
        parts.append(prod_prefix + "produce a good on this world")
        take("PRODUCE")
    if "PRODUCE_PRESTIGE" in codes:
        parts.append("+1 prestige if this world produced")
        take("PRODUCE_PRESTIGE")
    for kind, name in GOODS.items():
        wc = "WINDFALL_" + kind
        if wc in codes:
            parts.append(prod_prefix + f"produce on a {name} windfall world")
            take(wc)
    if "WINDFALL_ANY" in codes:
        parts.append(prod_prefix + "produce on any windfall world")
        take("WINDFALL_ANY")
    if "DRAW_IF" in codes:
        parts.append(f"draw {_n(v, 'card')} if this world produced")
        take("DRAW_IF")
    if "DRAW_EVERY_TWO" in codes:
        parts.append("draw 1 card per 2 goods produced")
        take("DRAW_EVERY_TWO")
    for kind, name in GOODS.items():
        dc = "DRAW_EACH_" + kind
        if dc in codes:
            parts.append(f"draw 1 card per {name} good produced")
            take(dc)
        mc = "DRAW_MOST_" + kind
        if mc in codes:
            parts.append(f"draw {_n(v, 'card')} if you produced the "
                         f"most {name} goods")
            take(mc)
    if "DRAW_WORLD_GENE" in codes:
        parts.append("draw 1 card per Genes world")
        take("DRAW_WORLD_GENE")
    if "DRAW_WORLD_RARE" in codes:
        parts.append("draw 1 card per Rare world")
        take("DRAW_WORLD_RARE")
    if "DRAW_MOST_PRODUCED" in codes:
        parts.append(f"draw {_n(v, 'card')} for the kind of good you "
                     "produced most")
        take("DRAW_MOST_PRODUCED")
    if "DRAW_DIFFERENT" in codes:
        parts.append("draw 1 card per kind of good produced")
        take("DRAW_DIFFERENT")
    if "SHIFT_RARE" in codes:
        parts.append("may move a Rare good to another world")
        take("SHIFT_RARE")

    # Per-card-type draws (mostly phase 1/5)
    for code, what in (("DRAW_CHROMO", "Chromosome card"),
                       ("DRAW_IMPERIUM", "Imperium card"),
                       ("DRAW_REBEL", "Rebel card"),
                       ("DRAW_REBEL_MILITARY", "Rebel military world"),
                       ("DRAW_XENO_MILITARY", "Xeno military world"),
                       ("DRAW_MILITARY", "Military world"),
                       ("DRAW_TWO_MILITARY", "2 Military worlds"),
                       ("DRAW_5_DEV", "5+ cost development"),
                       ("DRAW_LUCKY", "card if you name its cost")):
        if code in codes:
            if code == "DRAW_LUCKY":
                parts.append("name a number, draw the top card; keep "
                             "it if its cost matches")
            else:
                parts.append(f"draw 1 card per {what}")
            take(code)

    if "PRESTIGE" in codes:
        parts.append(f"+{_n(v, 'prestige')}")
        take("PRESTIGE")
    if "PRESTIGE_IF" in codes:
        parts.append("+1 prestige if conditions met")
        take("PRESTIGE_IF")
    if "PRESTIGE_REBEL" in codes:
        parts.append("+1 prestige when placing a Rebel military world")
        take("PRESTIGE_REBEL")
    if "PRESTIGE_SIX" in codes:
        parts.append("+1 prestige when placing a 6-cost development")
        take("PRESTIGE_SIX")
    if "PRESTIGE_MOST_CHROMO" in codes:
        parts.append("+1 prestige if you have the most Chromosome "
                     "cards")
        take("PRESTIGE_MOST_CHROMO")
    if "DISCARD_PRESTIGE" in codes:
        parts.append("may discard for +1 prestige")
        take("DISCARD_PRESTIGE")

    # Takeover powers
    for code, what in (("TAKEOVER_REBEL", "a Rebel military world"),
                       ("TAKEOVER_IMPERIUM", "an Imperium world"),
                       ("TAKEOVER_MILITARY", "a military world"),
                       ("TAKEOVER_PRESTIGE",
                        "a military world (spend prestige)")):
        if code in codes:
            parts.append(f"may take over {what}")
            take(code)
    if "TAKEOVER_DEFENSE" in codes:
        parts.append("defense bonus against takeovers")
        take("TAKEOVER_DEFENSE")
    if "PREVENT_TAKEOVER" in codes:
        parts.append("may prevent a takeover")
        take("PREVENT_TAKEOVER")
    if "NO_TAKEOVER" in codes:
        parts.append("cannot be taken over")
        take("NO_TAKEOVER")
    if "DESTROY" in codes:
        parts.append("destroy the world instead of taking it")
        take("DESTROY")

    if "ANTE_CARD" in codes:
        parts.append("may ante a card to gamble for another")
        take("ANTE_CARD")
    if "ORB_MOVEMENT" in codes:
        parts.append("Orb movement")
        take("ORB_MOVEMENT")
    if "NOT_THIS" in codes:
        take("NOT_THIS")  # modifier: power excludes this card itself

    # Anything not handled: show raw so nothing is hidden
    left = codes - handled
    if left:
        raw = " | ".join(sorted(left))
        parts.append(f"{raw}:{v}" + (f":{t}" if t else ""))

    if not parts:
        return None
    if prefix:
        # "discard this card to ..." reads as an imperative, so smooth
        # the first clause: "+3 Military" -> "gain +3 Military", and
        # "may place ..." -> "place ..." (drop the redundant "may").
        if parts[0][:1] in "+-":
            parts[0] = "gain " + parts[0]
        elif parts[0].startswith("may "):
            parts[0] = parts[0][4:]
    return prefix + "; ".join(parts)


VP_TEXT = {
    "THREE_VP": "per 3 VP in chips",
    "TOTAL_MILITARY": "per point of Military",
    "NEGATIVE_MILITARY": "per point of negative Military",
    "PRESTIGE": "per prestige",
    "GOODS": "per good at game end",
    "KIND_GOOD": "1/3/6/10 VP for 1/2/3/4 kinds of goods",
    "DEVEL": "per development",
    "WORLD": "per world",
    "SIX_DEVEL": "per 6-cost development",
    "MILITARY": "per Military world",
    "NONMILITARY_WORLD": "per non-Military world",
    "NONMILITARY_TRADE": "per non-Military world with a Trade power",
    "REBEL_FLAG": "per Rebel card",
    "REBEL_MILITARY": "per Rebel Military world",
    "IMPERIUM_FLAG": "per Imperium card",
    "ALIEN_FLAG": "per Alien card",
    "UPLIFT_FLAG": "per Uplift card",
    "CHROMO_FLAG": "per Chromosome world",
    "TERRAFORMING_FLAG": "per Terraforming card",
    "ANTI_XENO_FLAG": "per Anti-Xeno card",
    "ANTI_XENO_WORLD": "per Anti-Xeno world",
    "ANTI_XENO_DEVEL": "per Anti-Xeno development",
    "XENO_MILITARY": "per Xeno Military world",
    "NOVELTY_PRODUCTION": "per Novelty production world",
    "NOVELTY_WINDFALL": "per Novelty windfall world",
    "RARE_PRODUCTION": "per Rare production world",
    "RARE_WINDFALL": "per Rare windfall world",
    "GENE_PRODUCTION": "per Genes production world",
    "GENE_WINDFALL": "per Genes windfall world",
    "ALIEN_PRODUCTION": "per Alien production world",
    "ALIEN_WINDFALL": "per Alien windfall world",
    "ALIEN_TECHNOLOGY": "per Alien Technology card",
    "ALIEN_SCIENCE": "per Alien Science card",
    "ALIEN_UPLIFT": "per Alien/Uplift card",
    "DEVEL_EXPLORE": "per development with an Explore power",
    "DEVEL_TRADE": "per development with a Trade power",
    "DEVEL_CONSUME": "per development with a Consume power",
    "WORLD_EXPLORE": "per world with an Explore power",
    "WORLD_TRADE": "per world with a Trade power",
    "WORLD_CONSUME": "per world with a Consume power",
}


def card_to_text(block_lines):
    """Render one card's definition lines as readable text."""
    out = []
    flags = []
    good = None
    is_windfall = False
    vp_lines = []

    for raw in block_lines:
        if raw.startswith("F:"):
            flags = [f.strip() for f in raw[2:].split("|")]
            is_windfall = "WINDFALL" in flags

    has_bonus = any(l.startswith("V:") for l in block_lines)

    for raw in block_lines:
        if raw.startswith("T:"):
            t, cost, vp = [x.strip() for x in raw[2:].split(":")]
            kind = "Development" if t == "2" else "World"
            if kind == "World" and "MILITARY" in flags:
                head = f"Military world · defense {cost}"
            else:
                head = f"{kind} · cost {cost}"
            vp_disp = vp
            if has_bonus:
                vp_disp = f"{vp}+?" if vp != "0" else "?"
            out.append(f"{head} · {vp_disp} VP")
        elif raw.startswith("G:"):
            good = GOODS.get(raw[2:].strip(), raw[2:].strip())
        elif raw.startswith("P:"):
            fields = raw[2:].split(":")
            phase = int(fields[0])
            codes = fields[1]
            v = fields[2] if len(fields) > 2 else "0"
            t = fields[3] if len(fields) > 3 else "0"
            txt = power_text(phase, codes, v, t)
            if txt:
                out.append(f"{PHASES.get(phase, phase)}: {txt}")
        elif raw.startswith("V:"):
            fields = raw[2:].split(":", 2)
            pts = fields[0]
            vtype = fields[1]
            if vtype == "NAME":
                vp_lines.append(f"{pts} VP for {fields[2]}")
            else:
                vp_lines.append(
                    f"{pts} VP " + VP_TEXT.get(vtype, vtype)
                    if vtype != "KIND_GOOD" else VP_TEXT[vtype])

    if good:
        out.insert(1, f"Good: {good}"
                   + (" (windfall)" if is_windfall else ""))

    cats = [c for c in (FLAG_TEXT.get(f, f.title()) for f in flags)
            if c]
    if cats:
        out.insert(1, ", ".join(cats))

    out.extend(vp_lines)
    return "\n".join(out)
