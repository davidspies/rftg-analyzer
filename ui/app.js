/* RftG Analyzer UI */

const ACTION_NAMES = {
  0: "Search",
  1: "Explore +5",
  2: "Explore +1,+1",
  3: "Develop",
  4: "Develop (2nd)",
  5: "Settle",
  6: "Settle (2nd)",
  7: "Consume-Trade",
  8: "Consume-x2",
  9: "Produce",
};
function actionName(code) {
  if (code === -1 || code === undefined) return "";
  const prestige = code & 0x80;
  const base = ACTION_NAMES[code & 0x7f] || `action ${code}`;
  return prestige ? `Prestige ${base}` : base;
}

const $ = (id) => document.getElementById(id);

/* Card definitions from cards.txt, for hover tooltips */
let CARD_TEXT = {};
fetch("/api/cards")
  .then((r) => r.json())
  .then((t) => {
    CARD_TEXT = t;
  })
  .catch(() => {});

function cardTitle(name) {
  const t = CARD_TEXT[name];
  if (!t) return "";
  const esc = t
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/"/g, "&quot;")
    .replace(/\n/g, "&#10;");
  return ` title="${esc}"`;
}

let decisions = [];        // [{decision, logsBefore}]
let current = 0;
let activeGame = null;
let activeGameMeta = null;
let gameMetas = new Map();
const expandedDecisions = new Set();   // seqs showing all options

async function api(path, opts) {
  const resp = await fetch(path, opts);
  const data = await resp.json();
  if (data.error) throw new Error(data.error);
  return data;
}

function escapeHtml(s) {
  return String(s)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

function gameLabel(g) {
  if (g.source === "bga" && g.table_id !== undefined) {
    const suffix = g.perspective ? ` · ${g.perspective.name}` : "";
    return `BGA #${g.table_id}${suffix}`;
  }
  return g.id;
}

function bgaTableIdFromGame(id, meta) {
  if (meta?.source === "bga" && meta.table_id !== undefined)
    return String(meta.table_id);
  const m = id.match(/^bga-(\d+)(?:-p\d+)?$/);
  return m ? m[1] : null;
}

function bgaPerspectiveIdFromGame(id, meta) {
  if (meta?.perspective?.player_id !== undefined)
    return String(meta.perspective.player_id);
  const m = id.match(/^bga-\d+-p(\d+)$/);
  return m ? m[1] : null;
}

/* ---------- game list ---------- */

async function refreshGames() {
  const games = await api("/api/games");
  gameMetas = new Map(games.map((g) => [g.id, g]));
  if (activeGame) activeGameMeta = gameMetas.get(activeGame) || activeGameMeta;
  const ul = $("game-list");
  ul.innerHTML = "";
  for (const g of games) {
    const li = document.createElement("li");
    const res = g.result
      ? g.result.players
          .map((p) => `${p.name} ${p.vp}${p.winner ? "★" : ""}`)
          .join(" · ")
      : "";
    const head = document.createElement("div");
    head.textContent = gameLabel(g) + " ";
    const del = document.createElement("span");
    del.className = "del";
    del.title = "delete";
    del.textContent = "×";
    head.appendChild(del);
    const sub = document.createElement("div");
    sub.className = "sub";
    sub.textContent = `${g.decisions ?? "?"} decisions · ${res}`;
    li.appendChild(head);
    li.appendChild(sub);
    li.dataset.gameId = g.id;
    li.onclick = () => loadGame(g.id);
    del.onclick = async (e) => {
      e.stopPropagation();
      if (!confirm(`Delete ${gameLabel(g)}?`)) return;
      await api(`/api/delete/${g.id}`, { method: "POST" });
      if (activeGame === g.id) {
        activeGame = null;
        activeGameMeta = null;
        $("review").style.display = "none";
        $("empty-state").style.display = "block";
      }
      refreshGames();
    };
    if (g.id === activeGame) li.classList.add("active");
    ul.appendChild(li);
  }
}

async function loadGame(id, initialIndex = 0) {
  const events = await api(`/api/analysis/${id}`);
  activeGame = id;
  activeGameMeta = gameMetas.get(id) || null;
  document.querySelectorAll("#game-list li").forEach((el) => {
    el.classList.toggle("active", el.dataset.gameId === id);
  });

  decisions = [];
  let logs = [];
  for (const e of events) {
    if (e.event === "log") logs.push(e.text);
    else if (e.event === "decision") {
      decisions.push({ d: e, logsBefore: logs.join("") });
      logs = [];
    } else if (e.event === "result") {
      decisions.push({ result: e, logsBefore: logs.join("") });
      logs = [];
    }
  }
  current = Math.max(0, Math.min(decisions.length - 1, initialIndex));
  $("empty-state").style.display = "none";
  $("review").style.display = "block";
  $("nav-slider").max = decisions.length - 1;

  // Link to the original replay for BGA games
  const link = $("bga-link");
  const tableId = bgaTableIdFromGame(id, activeGameMeta);
  if (tableId) {
    link.href = `https://boardgamearena.com/gamereview?table=${tableId}`;
    link.style.display = "inline";
  } else {
    link.style.display = "none";
  }

  await configurePerspectiveControl();
  render();
}

function reviewedPlayerName() {
  for (const item of decisions) {
    if (!item.d) continue;
    const seat = seatOf(item.d);
    if (seat >= 0) return item.d.state.players[seat]?.name || null;
  }
  return null;
}

async function playersForBgaTable(tableId) {
  if (Array.isArray(activeGameMeta?.players)) return activeGameMeta.players;
  const data = await api(`/api/bga/players/${tableId}`);
  return data.players || [];
}

async function configurePerspectiveControl() {
  const wrap = $("perspective-wrap");
  const select = $("perspective-select");
  wrap.style.display = "none";
  select.disabled = false;
  select.innerHTML = "";
  select.onchange = null;

  const tableId = activeGame ? bgaTableIdFromGame(activeGame, activeGameMeta) : null;
  if (!tableId) return;

  let players;
  try {
    players = await playersForBgaTable(tableId);
  } catch (err) {
    alert(`Could not load BGA players: ${err.message}`);
    return;
  }
  if (!players.length) return;

  const reviewer = reviewedPlayerName();
  let selected = bgaPerspectiveIdFromGame(activeGame, activeGameMeta);
  if (!selected && reviewer) {
    const player = players.find((p) => p.name === reviewer);
    if (player) selected = String(player.id);
  }

  for (const p of players) {
    const opt = document.createElement("option");
    opt.value = String(p.id);
    opt.textContent = p.name;
    select.appendChild(opt);
  }
  if (selected) select.value = selected;

  select.onchange = async (e) => {
    const playerId = e.target.value;
    if (!playerId || playerId === bgaPerspectiveIdFromGame(activeGame, activeGameMeta))
      return;
    const keepIndex = current;
    select.disabled = true;
    try {
      const g = await api(
        `/api/bga/analyze/${tableId}?player_id=${encodeURIComponent(playerId)}`,
        { method: "POST" }
      );
      await refreshGames();
      await loadGame(g.id, keepIndex);
    } catch (err) {
      alert(err.message);
      select.disabled = false;
      if (selected) select.value = selected;
    }
  };
  wrap.style.display = "inline-flex";
}

/* ---------- option labels ---------- */

/* Phrase a chosen-cards set as whichever is shorter: the set itself or
 * its complement within the offered list ("discard A,B,C,D,E,F" with
 * one card kept becomes "keep G"). */
function fewerLabel(verb, keepVerb, items, offered) {
  const plain = items.length ? `${verb} ${items.join(", ")}` : `${verb} nothing`;
  if (!offered || !offered.length) return plain;
  const remaining = [...offered];
  for (const it of items) {
    const i = remaining.indexOf(it);
    if (i >= 0) remaining.splice(i, 1);
  }
  if (remaining.length < items.length)
    return remaining.length
      ? `${keepVerb} ${remaining.join(", ")}`
      : `${verb} all`;
  return plain;
}

function optionLabel(type, listItems, specialItems, offered) {
  if (type === "ACTION")
    return listItems
      .filter((a) => a !== -1)
      .map(actionName)
      .join(" + ");
  let label = listItems.length ? listItems.join(", ") : "(nothing)";
  if (type === "DISCARD")
    label = fewerLabel("discard", "keep", listItems, offered);
  if (type === "CONSUME_HAND")
    label = fewerLabel("consume", "keep", listItems, offered);
  if (type === "CONSUME" || type === "PRODUCE")
    label = listItems[0] === "none" || !listItems.length
      ? "(decline)"
      : `${listItems[0]} [power ${specialItems[0]}]`;
  if (type === "PLACE" && listItems[0] === "none") label = "(no build)";
  if (type === "DISCARD_PRODUCE" && specialItems.length)
    label += ` → ${specialItems.length ? specialItems.join(", ") : ""}`;
  if (type === "PAYMENT") {
    label = fewerLabel("pay", "pay all but", listItems, offered);
    if (specialItems.length) label += ` (using ${specialItems.join(", ")})`;
  }
  if (type === "START" && specialItems.length)
    label = `${specialItems[0]} — ` +
      fewerLabel("discard", "keep", listItems, offered);
  return label;
}

function chosenKey(c, type) {
  // Build a comparable key for the chosen answer
  if (type === "PLACE")
    return JSON.stringify([[c.rv === -1 ? "none" : null], []]);
  return JSON.stringify([c.list, c.special]);
}

/* ---------- rendering ---------- */

function render() {
  const item = decisions[current];
  $("nav-pos").textContent = `${current + 1} / ${decisions.length}`;
  $("nav-slider").value = current;
  $("log").textContent = item.logsBefore || "(game start)";

  if (item.result) {
    $("decision-title").innerHTML = "Game over";
    const lines = item.result.players
      .map((p) => `${p.name}: ${p.vp} VP${p.winner ? " — winner" : ""}`)
      .join("<br>");
    $("options").innerHTML = `<div>${lines}</div>`;
    $("state").innerHTML = "";
    return;
  }

  const d = item.d;
  renderDecision(d);
  renderState(d.state);
}

function renderDecision(d) {
  const opts = [...d.options];
  opts.sort((a, b) => b.score - a.score);

  // Identify the chosen option among the scored ones
  let chosenIdx = -1;
  for (let i = 0; i < opts.length; i++) {
    const o = opts[i];
    let isChosen = false;
    if (d.type === "ACTION") {
      const chosenActs = d.chosen.list.filter((a) => a !== -1);
      const optActs = o.list.filter((a) => a !== -1);
      isChosen =
        chosenActs.length === optActs.length &&
        chosenActs.every((a) => optActs.includes(a));
    } else if (d.chosen.rv_name !== undefined) {
      isChosen = o.list.length === 1 && o.list[0] === d.chosen.rv_name;
    } else {
      isChosen =
        JSON.stringify(o.list) === JSON.stringify(d.chosen.list) &&
        JSON.stringify(o.special) === JSON.stringify(d.chosen.special);
    }
    if (isChosen) { chosenIdx = i; break; }
  }

  const best = opts.length ? opts[0].score : 0;
  const chosenScore = chosenIdx >= 0 ? opts[chosenIdx].score : null;
  const loss = chosenScore !== null ? best - chosenScore : null;

  let verdict = "";
  if (loss !== null && opts.length > 1) {
    if (loss < 0.005) verdict = `<span class="verdict-good">✓ best move</span>`;
    else
      verdict = `<span class="verdict-bad">−${(loss * 100).toFixed(1)}% vs best</span>`;
  }
  d._loss = loss; // for mistake filter

  const who = d.state.players[seatOf(d)]?.name || `player ${d.player}`;
  $("decision-title").innerHTML =
    `Round ${d.state.round} · <b>${d.type}</b> · ${who} ${verdict}`;

  const box = $("options");
  box.innerHTML = "";
  if (!opts.length) {
    const chosenLbl = optionLabel(d.type, d.chosen.list, d.chosen.special, d.offered);
    box.innerHTML = `<div class="sub">No scores for this decision type.
      Chose: <b>${chosenLbl}</b></div>`;
    return;
  }
  const expanded = expandedDecisions.has(d.seq);
  const shown = expanded ? opts : opts.slice(0, 12);
  for (let i = 0; i < shown.length; i++) {
    const o = shown[i];
    const div = document.createElement("div");
    div.className = "opt" + (i === 0 ? " best" : "") +
      (i === chosenIdx ? " chosen" : "");
    const pct = Math.max(0, Math.min(1, o.score));
    const width = pct * 100;
    div.innerHTML =
      `<div class="label" title="${optionLabel(d.type, o.list, o.special, d.offered)}">` +
      `${optionLabel(d.type, o.list, o.special, d.offered)}</div>` +
      `<div class="bar-wrap"><div class="bar" style="width:${width}%"></div>` +
      `<span class="pct">${(pct * 100).toFixed(1)}%</span></div>` +
      `<span class="tags">${i === chosenIdx ? '<span class="tag you">you</span>' : ""}` +
      `${i === 0 ? '<span class="tag ai">AI pick</span>' : ""}</span>`;
    box.appendChild(div);
  }
  if (opts.length > 12) {
    const more = document.createElement("div");
    more.className = "more-toggle";
    more.textContent = expanded
      ? "show fewer options"
      : `show ${opts.length - shown.length} more options`;
    more.onclick = () => {
      if (expanded) expandedDecisions.delete(d.seq);
      else expandedDecisions.add(d.seq);
      render();
    };
    box.appendChild(more);
  }

  renderPredictions(d, box);
}

/* Show the opponent action(s) the AI predicted while scoring this
 * decision — it is guessing from the board, not seeing the real pick. */
function renderPredictions(d, box) {
  if (d.type !== "ACTION" || !d.predictions?.length) return;

  // Group by predicted player (multiplayer can have several opponents)
  const byPlayer = new Map();
  for (const p of d.predictions) {
    if (!byPlayer.has(p.player)) byPlayer.set(p.player, []);
    byPlayer.get(p.player).push(p);
  }

  const wrap = document.createElement("div");
  wrap.className = "predictions";
  for (const [pid, list] of byPlayer) {
    const name = d.state.players[pid]?.name || `player ${pid}`;
    list.sort((a, b) => b.prob - a.prob);
    const expanded = expandedDecisions.has(`pred-${d.seq}-${pid}`);
    const shown = expanded ? list : list.slice(0, 4);
    let rows = shown
      .map((p) => {
        const label = p.actions.map(actionName).join(" + ");
        const w = (p.prob * 100).toFixed(0);
        return (
          `<div class="pred-row"><div class="pred-label">${label}</div>` +
          `<div class="bar-wrap"><div class="bar pred-bar" ` +
          `style="width:${p.prob * 100}%"></div>` +
          `<span class="pct">${w}%</span></div></div>`
        );
      })
      .join("");
    if (list.length > 4) {
      const lbl = expanded
        ? "show fewer"
        : `show ${list.length - shown.length} more`;
      rows += `<div class="more-toggle pred-more" data-pid="${pid}">${lbl}</div>`;
    }
    wrap.innerHTML +=
      `<div class="pred-head">AI's predicted move for <b>${name}</b> ` +
      `</div>${rows}`;
  }
  box.appendChild(wrap);
  wrap.querySelectorAll(".pred-more").forEach((el) => {
    el.onclick = () => {
      const key = `pred-${d.seq}-${el.dataset.pid}`;
      if (expandedDecisions.has(key)) expandedDecisions.delete(key);
      else expandedDecisions.add(key);
      render();
    };
  });
}

function seatOf(d) {
  // state.players is in seat order; find the reviewed player's seat by
  // matching the hand presence (only the reviewed player has "hand")
  return d.state.players.findIndex((p) => p.hand !== undefined);
}

function renderState(state) {
  const box = $("state");
  box.innerHTML = "";

  if (state.goals?.length) {
    const strip = document.createElement("div");
    strip.className = "goal-strip";
    strip.textContent =
      "Goals: " +
      state.goals
        .map((g) => g.name + (g.avail ? "" : " (claimed)"))
        .join(" · ");
    box.appendChild(strip);
  }

  for (const p of state.players) {
    const div = document.createElement("div");
    div.className = "player-board" + (p.hand !== undefined ? " me" : "");
    const acts = p.actions
      .filter((a) => a !== -1)
      .map(actionName)
      .join(", ");
    const cards = p.tableau
      .map(
        (c) =>
          `<span class="card${c.dev ? " dev" : ""}"${cardTitle(c.name)}>${c.name}` +
          `${c.goods ? ` <span class="goods">●${c.goods > 1 ? "×" + c.goods : ""}</span>` : ""}</span>`
      )
      .join("");
    let hand = "";
    if (p.hand !== undefined) {
      hand =
        `<div class="hand-label">Hand:</div><div class="cards">` +
        p.hand
          .map((c) => `<span class="card"${cardTitle(c)}>${c}</span>`)
          .join("") +
        `</div>`;
    }
    div.innerHTML =
      `<h3>${p.name}</h3>` +
      `<div class="stats">${p.vp} VP · military ${p.military} · ` +
      `hand ${p.hand_size}${p.prestige ? ` · prestige ${p.prestige}` : ""}` +
      `${acts ? ` · chose: ${acts}` : ""}</div>` +
      `<div class="cards">${cards || "(empty tableau)"}</div>` +
      hand;
    box.appendChild(div);
  }
}

/* ---------- navigation ---------- */

function go(idx) {
  current = Math.max(0, Math.min(decisions.length - 1, idx));
  render();
}

function nextMistake(dir) {
  let i = current + dir;
  while (i >= 0 && i < decisions.length) {
    const it = decisions[i];
    if (it.d) {
      // compute loss lazily by rendering logic
      const opts = it.d.options;
      if (opts?.length > 1) {
        const best = Math.max(...opts.map((o) => o.score));
        const chosen = findChosenScore(it.d);
        if (chosen !== null && best - chosen >= 0.02) break;
      }
    }
    i += dir;
  }
  if (i >= 0 && i < decisions.length) go(i);
}

function findChosenScore(d) {
  for (const o of d.options) {
    if (d.type === "ACTION") {
      const a = d.chosen.list.filter((x) => x !== -1);
      const b = o.list.filter((x) => x !== -1);
      if (a.length === b.length && a.every((x) => b.includes(x)))
        return o.score;
    } else if (d.chosen.rv_name !== undefined) {
      if (o.list.length === 1 && o.list[0] === d.chosen.rv_name)
        return o.score;
    } else if (
      JSON.stringify(o.list) === JSON.stringify(d.chosen.list) &&
      JSON.stringify(o.special) === JSON.stringify(d.chosen.special)
    )
      return o.score;
  }
  return null;
}

$("prev").onclick = () =>
  $("only-mistakes").checked ? nextMistake(-1) : go(current - 1);
$("next").onclick = () =>
  $("only-mistakes").checked ? nextMistake(1) : go(current + 1);
$("nav-slider").oninput = (e) => go(+e.target.value);
document.addEventListener("keydown", (e) => {
  if (e.key === "ArrowLeft") $("prev").click();
  if (e.key === "ArrowRight") $("next").click();
});

/* ---------- demo + BGA ---------- */

$("demo-form").onsubmit = async (e) => {
  e.preventDefault();
  const btn = e.target.querySelector("button");
  btn.disabled = true;
  btn.textContent = "Generating…";
  try {
    const seed = Math.floor(Math.random() * 100000);
    const g = await api(
      `/api/demo?players=${$("demo-players").value}` +
        `&expansion=${$("demo-exp").value}&seed=${seed}`,
      { method: "POST" }
    );
    await refreshGames();
    await loadGame(g.id);
  } catch (err) {
    alert(err.message);
  } finally {
    btn.disabled = false;
    btn.textContent = "New demo game";
  }
};

let bgaPage = 0;

async function loadBgaPage() {
  const data = await api(`/api/bga/sync?page=${bgaPage + 1}`,
                         { method: "POST" });
  bgaPage++;
  const analyzed = new Set(
    (await api("/api/games"))
      .map((g) => bgaTableIdFromGame(g.id, g))
      .filter(Boolean));
  const box = $("bga-tables");
  if (bgaPage === 1) box.innerHTML = "<b>BGA games</b>";
  const moreBtn = $("bga-more");
  if (moreBtn) moreBtn.remove();
  renderBgaTables(box, data.tables, analyzed);
  if (data.tables.length === 10) {
    const more = document.createElement("div");
    more.className = "more-toggle";
    more.id = "bga-more";
    more.textContent = "show 10 more games";
    more.onclick = () => {
      more.textContent = "loading…";
      loadBgaPage().catch((e) => alert(e.message));
    };
    box.appendChild(more);
  }
}

function renderBgaTables(box, tables, analyzed) {
  for (const t of tables) {
      const div = document.createElement("div");
      div.className = "tbl";
      const id = t.table_id || t.id;
      const names = (t.player_names || "").split(",").join(", ");
      const scores = t.scores || "";
      const when = t.start
        ? new Date(+t.start * 1000).toLocaleString(undefined, {
            month: "short", day: "numeric",
            hour: "2-digit", minute: "2-digit" })
        : "";
      const done = analyzed.has(String(id));
      const head = `<div>${escapeHtml(names)}${done ? " ✓" : ""}</div>`;
      const sub = `<div class="sub">${escapeHtml(when)} · ${escapeHtml(scores)} · #${escapeHtml(id)}</div>`;
      div.innerHTML = head + sub;
      const status = document.createElement("div");
      status.className = "sub";
      div.appendChild(status);
      div.onclick = async () => {
        status.textContent = "analyzing…";
        try {
          const g = await api(`/api/bga/analyze/${id}`, { method: "POST" });
          await refreshGames();
          await loadGame(g.id);
          status.textContent = "✓ analyzed";
        } catch (err) {
          status.textContent = `✗ ${err.message}`;
        }
      };
      box.appendChild(div);
    }
}

$("btn-sync").onclick = async () => {
  const btn = $("btn-sync");
  btn.disabled = true;
  btn.textContent = "Syncing…";
  try {
    bgaPage = 0;
    await loadBgaPage();
  } catch (err) {
    alert(err.message);
  } finally {
    btn.disabled = false;
    btn.textContent = "Sync BGA games";
  }
};

/* Deep links: #game-id or #game-id/decision-index */
async function initFromHash() {
  await refreshGames();
  const hash = location.hash.slice(1);
  if (!hash) return;
  const [id, idx] = hash.split("/");
  try {
    await loadGame(id);
    if (idx) go(+idx);
  } catch (e) {
    /* stale link; ignore */
  }
}

initFromHash();
