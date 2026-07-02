/*
 * Race for the Galaxy game analyzer.
 *
 * Replays a scripted game (produced by the BGA log parser) through
 * Keldon Jones' RftG engine, and at each decision point of the player
 * under review, asks the neural-network AI to score every available
 * option.  Emits a JSON-lines stream describing each decision, the
 * options with their estimated win probabilities, and the game state.
 *
 * Script format: see README.md in this directory.
 */

#include "rftg.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* Maximum scripted entries */
#define MAX_SCRIPT 4096

/* Maximum candidates scored per decision */
#define MAX_CANDIDATES 512

/* Maximum tokens per script line */
#define MAX_TOKENS 64

/*
 * One scripted entry (choice, draw, or expectation) for one player.
 */
typedef struct entry
{
	/* Entry kind */
	enum { ENTRY_CHOICE, ENTRY_EXPECT } kind;

	/* Optional entry: skipped if the engine does not ask a choice of
	 * this type (used for decisions the engine auto-resolves when
	 * only one option exists) */
	int optional;

	/* Choice type (CHOICE_* constant) */
	int type;

	/* Raw tokens of the answer (card names or numbers) */
	char *tokens[MAX_TOKENS];
	int num_tokens;

	/* Index where special items start (after ":"), or -1 */
	int special_at;

	/* Script line number (for error messages) */
	int line;

	/* Entry has been consumed (matched, or passed over for good) */
	int consumed;

} entry;

/* Scripted entries, per player */
static entry script[MAX_PLAYER][MAX_SCRIPT];
static int script_len[MAX_PLAYER];
static int script_pos[MAX_PLAYER];

/* Scripted draws, per player (design pointers, NULL = unknown) */
static design *draw_script[MAX_PLAYER][MAX_DECK];
static int draw_len[MAX_PLAYER];

/* Forced active goals */
static int forced_goal[MAX_GOAL];
static int num_forced_goal;

/* Scripted start world options per player */
static design *start_options[MAX_PLAYER][2];

/* Expected final scores (-1 = unchecked) */
static int final_score[MAX_PLAYER];

/* Expects held until the next ACTION ask: round starts are the only
 * points where BGA's running counts and engine state align */
static entry *pending_expect[MAX_PLAYER][64];
static int num_pending[MAX_PLAYER];

/* Campaign structure used to force scripted draws */
static campaign analyzer_camp;

/* Player under review (original script index) */
static int review_player = 0;

/* Map from current seat position to original script index */
static int orig_of[MAX_PLAYER];

/* Whether to compute option scores (slow) */
static int score_options = 1;

/* Game was conceded: end gracefully when a player's script runs out */
static int concede_game;
static int conceded_here;

/* The game being replayed */
static game real_game;

/* Sequence number of emitted decisions */
static int decision_seq;

/* Buffered engine messages to attach to next emitted event */
static char msg_buf[16384];

/* Verbose mode: dump engine messages to stderr as they happen */
static int verbose;

/*
 * Collected candidate scores for the current decision.
 */
typedef struct candidate
{
	int list[MAX_DECK];
	int num;
	int special[MAX_DECK];
	int num_special;
	double score;
} candidate;

static candidate cand[MAX_CANDIDATES];
static int num_cand;

/*
 * Predicted opponent action distribution for the current decision.
 */
typedef struct prediction
{
	int who;        /* opponent seat index (matches state.players order) */
	int act0;       /* first action code */
	int act1;       /* second action code, or -1 */
	double prob;    /* probability the net assigns this combo */
} prediction;

#define MAX_PREDICTION 256
static prediction preds[MAX_PREDICTION];
static int num_pred;

/* Set by enumerate_candidates when it exhaustively enumerated the
 * played answer's size: in that case the played answer MUST appear
 * among the scored candidates, and the main loop asserts so. */
static int enum_exhaustive;

/*
 * Frontend callbacks required by the engine.
 */
void display_error(char *msg)
{
	fprintf(stderr, "%s", msg);
}

void message_add(game *g, char *msg)
{
	/* Append to buffer */
	if (strlen(msg_buf) + strlen(msg) + 1 < sizeof(msg_buf))
	{
		strcat(msg_buf, msg);
	}

	/* Echo in verbose mode */
	if (verbose) fprintf(stderr, "%s", msg);
}

void message_add_formatted(game *g, char *msg, char *tag)
{
	message_add(g, msg);
}

int game_rand(game *g)
{
	return simple_rand(&g->random_seed);
}

/*
 * Print a JSON-escaped string (including quotes).
 */
static void json_str(const char *s)
{
	putchar('"');
	for (; *s; s++)
	{
		switch (*s)
		{
			case '"': fputs("\\\"", stdout); break;
			case '\\': fputs("\\\\", stdout); break;
			case '\n': fputs("\\n", stdout); break;
			case '\t': fputs("\\t", stdout); break;
			default:
				if ((unsigned char)*s < 0x20)
					printf("\\u%04x", *s);
				else
					putchar(*s);
		}
	}
	putchar('"');
}

/*
 * Return a card's name, or a placeholder for pseudo-indices (the
 * engine uses -1 entries for e.g. the prestige bonus consume power).
 */
static const char *card_name(game *g, int which)
{
	if (which < 0 || which >= g->deck_size) return "none";
	return g->deck[which].d_ptr->name;
}

/*
 * Names of choice types, indexed by CHOICE_* constant.
 */
static const char *choice_name[] =
{
	"ACTION", "START", "DISCARD", "SAVE", "DISCARD_PRESTIGE",
	"PLACE", "PAYMENT", "SETTLE", "TAKEOVER", "DEFEND",
	"TAKEOVER_PREVENT", "UPGRADE", "TRADE", "CONSUME", "CONSUME_HAND",
	"GOOD", "LUCKY", "ANTE", "KEEP", "WINDFALL", "PRODUCE",
	"DISCARD_PRODUCE", "SEARCH_TYPE", "SEARCH_KEEP", "OORT_KIND"
};

#define NUM_CHOICE_NAMES (sizeof(choice_name) / sizeof(choice_name[0]))

/*
 * Look up a choice type by name; -1 if unknown.
 */
static int choice_by_name(const char *name)
{
	int i;

	for (i = 0; i < NUM_CHOICE_NAMES; i++)
	{
		if (!strcmp(name, choice_name[i])) return i;
	}

	return -1;
}

/*
 * Look up a card design by name; NULL if unknown.  A few names have
 * expansion-specific variants, so prefer a design that actually has
 * copies in the current expansion's deck.
 */
static design *design_by_name(const char *name)
{
	design *fallback = NULL;
	int i;

	for (i = 0; i < MAX_DESIGN; i++)
	{
		if (!library[i].name || strcmp(library[i].name, name))
			continue;

		/* Prefer designs present in this expansion */
		if (library[i].expand[real_game.expanded] > 0)
			return &library[i];

		fallback = &library[i];
	}

	return fallback;
}

/*
 * Look up a goal by name; -1 if unknown.
 */
static int goal_by_name(const char *name)
{
	int i;

	for (i = 0; i < MAX_GOAL; i++)
	{
		if (!strcmp(goal_name[i], name)) return i;
	}

	return -1;
}

/*
 * Fail with a message.
 */
static void die(const char *fmt, const char *arg, int line)
{
	fprintf(stderr, "analyzer: ");
	fprintf(stderr, fmt, arg);
	if (line) fprintf(stderr, " (script line %d)", line);
	fprintf(stderr, "\n");
	exit(1);
}

/*
 * Tokenize a script line.  Tokens are whitespace-separated words or
 * double-quoted strings.  Returns number of tokens.  Token memory is
 * strdup'ed.
 */
static int tokenize(char *line, char *tokens[], int max)
{
	int n = 0;
	char *p = line, *start;
	char buf[1024];

	while (*p)
	{
		/* Skip whitespace */
		while (*p && isspace((unsigned char)*p)) p++;

		/* Check for end or comment */
		if (!*p || *p == '#') break;

		/* Check for quoted token */
		if (*p == '"')
		{
			/* Find closing quote */
			start = ++p;
			while (*p && *p != '"') p++;

			/* Copy token */
			memcpy(buf, start, p - start);
			buf[p - start] = 0;

			/* Skip closing quote */
			if (*p) p++;
		}
		else
		{
			/* Find end of word */
			start = p;
			while (*p && !isspace((unsigned char)*p)) p++;

			/* Copy token */
			memcpy(buf, start, p - start);
			buf[p - start] = 0;
		}

		/* Store token */
		if (n < max) tokens[n++] = strdup(buf);
	}

	return n;
}

/*
 * Load the game script.
 */
static void load_script(const char *path)
{
	FILE *fff;
	char line[1024];
	char *tokens[MAX_TOKENS];
	int num_tokens;
	int lineno = 0;
	int i, p, type;
	entry *e_ptr;
	design *d_ptr;

	/* Defaults */
	real_game.num_players = 2;
	real_game.expanded = 0;
	real_game.advanced = 0;
	real_game.promo = 0;
	real_game.goal_disabled = 0;
	real_game.takeover_disabled = 0;
	real_game.random_seed = 12345;

	/* Open script */
	fff = fopen(path, "r");
	if (!fff) die("cannot open script %s", path, 0);

	/* Read lines */
	while (fgets(line, sizeof(line), fff))
	{
		lineno++;

		/* Tokenize */
		num_tokens = tokenize(line, tokens, MAX_TOKENS);

		/* Skip empty lines */
		if (!num_tokens) continue;

		/* Header directives */
		if (!strcmp(tokens[0], "players"))
		{
			real_game.num_players = atoi(tokens[1]);
		}
		else if (!strcmp(tokens[0], "expanded"))
		{
			real_game.expanded = atoi(tokens[1]);
		}
		else if (!strcmp(tokens[0], "advanced"))
		{
			real_game.advanced = atoi(tokens[1]);
		}
		else if (!strcmp(tokens[0], "promo"))
		{
			real_game.promo = atoi(tokens[1]);
		}
		else if (!strcmp(tokens[0], "goals"))
		{
			real_game.goal_disabled = !atoi(tokens[1]);
		}
		else if (!strcmp(tokens[0], "takeovers"))
		{
			real_game.takeover_disabled = !atoi(tokens[1]);
		}
		else if (!strcmp(tokens[0], "seed"))
		{
			real_game.random_seed = atoi(tokens[1]);
		}
		else if (!strcmp(tokens[0], "review"))
		{
			review_player = atoi(tokens[1]);
		}
		else if (!strcmp(tokens[0], "concede"))
		{
			concede_game = atoi(tokens[1]);
		}
		else if (!strcmp(tokens[0], "finalscore"))
		{
			p = atoi(tokens[1]);
			final_score[p] = atoi(tokens[2]);
		}
		else if (!strcmp(tokens[0], "score"))
		{
			score_options = atoi(tokens[1]);
		}
		else if (!strcmp(tokens[0], "name"))
		{
			p = atoi(tokens[1]);
			real_game.p[p].name = strdup(tokens[2]);
		}
		else if (!strcmp(tokens[0], "goal"))
		{
			i = goal_by_name(tokens[1]);
			if (i < 0) die("unknown goal \"%s\"", tokens[1],
			               lineno);
			forced_goal[num_forced_goal++] = i;
		}
		else if (!strcmp(tokens[0], "startoptions"))
		{
			/* Both dealt start world candidates */
			p = atoi(tokens[1]);
			for (i = 0; i < 2; i++)
			{
				d_ptr = design_by_name(tokens[2 + i]);
				if (!d_ptr) die("unknown card \"%s\"",
				                tokens[2 + i], lineno);
				start_options[p][i] = d_ptr;
			}
		}
		else if (!strcmp(tokens[0], "draw"))
		{
			/* Scripted draw */
			p = atoi(tokens[1]);

			if (!strcmp(tokens[2], "?"))
			{
				/* Unknown draw */
				d_ptr = NULL;
			}
			else
			{
				/* Look up design */
				d_ptr = design_by_name(tokens[2]);
				if (!d_ptr) die("unknown card \"%s\"",
				                tokens[2], lineno);
			}

			/* Add to draw script */
			draw_script[p][draw_len[p]++] = d_ptr;
		}
		else if (!strcmp(tokens[0], "expect") ||
		         !strcmp(tokens[0], "choice") ||
		         !strcmp(tokens[0], "choice?"))
		{
			/* Scripted choice or expectation */
			p = atoi(tokens[1]);

			/* Get next entry */
			e_ptr = &script[p][script_len[p]++];
			e_ptr->line = lineno;
			e_ptr->special_at = -1;
			e_ptr->optional = !strcmp(tokens[0], "choice?");

			if (!strcmp(tokens[0], "expect"))
			{
				/* Expectation */
				e_ptr->kind = ENTRY_EXPECT;
				e_ptr->type = 0;

				/* Copy remaining tokens */
				for (i = 2; i < num_tokens; i++)
					e_ptr->tokens[e_ptr->num_tokens++] =
						tokens[i];
				continue;
			}

			/* Choice */
			e_ptr->kind = ENTRY_CHOICE;

			/* Look up type */
			type = choice_by_name(tokens[2]);
			if (type < 0) die("unknown choice type %s",
			                  tokens[2], lineno);
			e_ptr->type = type;

			/* Copy answer tokens */
			for (i = 3; i < num_tokens; i++)
			{
				/* Check for special separator */
				if (!strcmp(tokens[i], ":"))
				{
					e_ptr->special_at = e_ptr->num_tokens;
					continue;
				}

				e_ptr->tokens[e_ptr->num_tokens++] =
					tokens[i];
			}
		}
		else
		{
			die("unknown directive %s", tokens[0], lineno);
		}
	}

	fclose(fff);
}

/* Positions of the offered list already used by the current answer
 * (the same card name can appear twice, e.g. two copies in hand) */
static char used_pos[MAX_DECK];

/* When set, resolution failures return errors instead of dying (used
 * to skip optional entries whose content no longer matches) */
static int lenient_resolve;
static int resolve_failed;

/*
 * Reset the used-position tracking before translating an answer.
 */
static void reset_used(void)
{
	memset(used_pos, 0, sizeof(used_pos));
}

/*
 * Resolve a card name to a deck index from the offered list, consuming
 * one unused position.  Returns -1 if not resolvable.
 */
static int resolve_card(game *g, const char *name, int offered[], int num)
{
	int i;

	/* Check for "none" */
	if (!strcmp(name, "none")) return -1;

	/* Look in offered list */
	for (i = 0; i < num; i++)
	{
		if (used_pos[i]) continue;
		if (!strcmp(name, "?") ||
		    !strcmp(g->deck[offered[i]].d_ptr->name, name))
		{
			/* Consume this position */
			used_pos[i] = 1;
			return offered[i];
		}
	}

	/* Not offered */
	if (lenient_resolve)
	{
		resolve_failed = 1;
		return -1;
	}

	/* Report desync loudly */
	fprintf(stderr, "analyzer: \"%s\" not among offered:", name);
	for (i = 0; i < num; i++)
		fprintf(stderr, " \"%s\"", g->deck[offered[i]].d_ptr->name);
	fprintf(stderr, "\n");

	return -1;
}

/*
 * Emit the current game state as a JSON object (without surrounding
 * braces' key).
 */
static void emit_state(game *g)
{
	player *p_ptr;
	card *c_ptr;
	int i, x, n;

	printf("{\"round\":%d,\"cur_action\":%d,\"vp_pool\":%d,", g->round,
	       g->cur_action, g->vp_pool);

	/* Count draw and discard piles */
	n = 0;
	for (i = 0; i < g->deck_size; i++)
		if (g->deck[i].where == WHERE_DECK) n++;
	printf("\"deck\":%d,", n);
	n = 0;
	for (i = 0; i < g->deck_size; i++)
		if (g->deck[i].where == WHERE_DISCARD) n++;
	printf("\"discard\":%d,", n);

	/* Goals */
	printf("\"goals\":[");
	n = 0;
	for (i = 0; i < MAX_GOAL; i++)
	{
		if (!g->goal_active[i]) continue;
		if (n++) putchar(',');
		printf("{\"name\":");
		json_str(goal_name[i]);
		printf(",\"avail\":%d}", g->goal_avail[i]);
	}
	printf("],");

	/* Players */
	printf("\"players\":[");
	for (i = 0; i < g->num_players; i++)
	{
		p_ptr = &g->p[i];
		if (i) putchar(',');
		printf("{\"name\":");
		json_str(p_ptr->name ? p_ptr->name : "?");
		printf(",\"vp\":%d,\"prestige\":%d,\"military\":%d,",
		       p_ptr->vp, p_ptr->prestige, total_military(g, i));
		printf("\"hand_size\":%d,",
		       count_player_area(g, i, WHERE_HAND));

		/* Hand contents (review player only) */
		if (orig_of[i] == review_player)
		{
			printf("\"hand\":[");
			n = 0;
			x = p_ptr->head[WHERE_HAND];
			for ( ; x != -1; x = g->deck[x].next)
			{
				if (n++) putchar(',');
				json_str(g->deck[x].d_ptr->name);
			}
			printf("],");
		}

		/* Actions chosen this round (if any) */
		printf("\"actions\":[%d,%d],", p_ptr->action[0],
		       p_ptr->action[1]);

		/* Tableau */
		printf("\"tableau\":[");
		n = 0;
		x = p_ptr->head[WHERE_ACTIVE];
		for ( ; x != -1; x = g->deck[x].next)
		{
			c_ptr = &g->deck[x];
			if (n++) putchar(',');
			printf("{\"name\":");
			json_str(c_ptr->d_ptr->name);
			printf(",\"goods\":%d,\"dev\":%d}",
			       c_ptr->num_goods,
			       c_ptr->d_ptr->type == TYPE_DEVELOPMENT);
		}
		printf("]}");
	}
	printf("]}");
}

/*
 * Flush buffered engine messages as a log event.
 */
static void flush_messages(void)
{
	if (!msg_buf[0]) return;

	printf("{\"event\":\"log\",\"text\":");
	json_str(msg_buf);
	printf("}\n");

	msg_buf[0] = 0;
}

/*
 * Analyzer hook: collect option scores from the AI's choice handlers.
 */
static void collect_hook(game *g, int who, int type, int list[], int num,
                         int special[], int num_special, double score)
{
	candidate *c_ptr;
	int i;

	/* Check for room */
	if (num_cand >= MAX_CANDIDATES) return;

	/* Get next candidate */
	c_ptr = &cand[num_cand++];

	/* Copy items */
	c_ptr->num = num;
	for (i = 0; i < num; i++) c_ptr->list[i] = list[i];
	c_ptr->num_special = num_special;
	for (i = 0; i < num_special; i++) c_ptr->special[i] = special[i];
	c_ptr->score = score;
}

/*
 * Collect a predicted opponent action (installed during ACTION scoring).
 * Reports the opponent by original script index so the UI can name them.
 */
static void collect_predict(game *g, int who, int act0, int act1,
                            double prob)
{
	prediction *p_ptr;

	if (num_pred >= MAX_PREDICTION) return;
	if (prob <= 0.0005) return;   /* drop negligible combos */

	p_ptr = &preds[num_pred++];
	/* who is a seat index; state.players is emitted in seat order, so
	 * store the seat directly for the UI to index */
	p_ptr->who = who;
	p_ptr->act0 = act0;
	p_ptr->act1 = act1;
	p_ptr->prob = prob;
}

/*
 * Add a candidate scored via ai_eval_choice.
 */
/*
 * Sort an int array ascending (small arrays; insertion sort).
 */
static void sort_ints(int a[], int n)
{
	int i, j, v;

	for (i = 1; i < n; i++)
	{
		v = a[i];
		for (j = i - 1; j >= 0 && a[j] > v; j--) a[j + 1] = a[j];
		a[j + 1] = v;
	}
}

/*
 * True if a stored candidate is the same answer as (list/num,
 * special/ns).  Candidate arrays are kept in canonical (sorted) order,
 * and callers pass sorted arrays here, so a plain ordered comparison
 * suffices.
 */
static int same_answer(candidate *c, int list[], int num,
                       int special[], int ns)
{
	int j;

	if (c->num != num || c->num_special != ns) return 0;
	for (j = 0; j < num; j++)
		if (c->list[j] != list[j]) return 0;
	for (j = 0; j < ns; j++)
		if (c->special[j] != special[j]) return 0;
	return 1;
}

/*
 * True if an answer is already among the scored candidates.
 */
static int answer_present(int list[], int num, int special[], int ns)
{
	int i;

	for (i = 0; i < num_cand; i++)
		if (same_answer(&cand[i], list, num, special, ns)) return 1;
	return 0;
}

static void add_candidate(game *g, int who, int type, int list[], int num,
                          int special[], int ns, int arg1, int arg2,
                          int arg3)
{
	candidate *c_ptr;
	double score;
	int i;
	int slist[MAX_DECK], sspec[MAX_DECK];

	/* Check for room */
	if (num_cand >= MAX_CANDIDATES) return;

	/* Canonicalize: the list and special arrays are sets of cards, so
	 * store and compare them in sorted order (the same set may arrive
	 * in different orders from enumeration vs the engine's answer).
	 * Scoring still uses the original order, in case a power attaches
	 * meaning to it. */
	for (i = 0; i < num; i++) slist[i] = list[i];
	for (i = 0; i < ns; i++) sspec[i] = special[i];
	sort_ints(slist, num);
	sort_ints(sspec, ns);

	/* Skip candidates already scored (e.g. the actual answer when
	 * enumeration already covered it) */
	if (answer_present(slist, num, sspec, ns)) return;

	/* Score candidate */
	score = ai_eval_choice(g, who, type, list, num, special, ns,
	                       arg1, arg2, arg3);

	/* Skip illegal candidates */
	if (score <= -3) return;

	/* Store candidate (canonical order) */
	c_ptr = &cand[num_cand++];
	c_ptr->num = num;
	for (i = 0; i < num; i++) c_ptr->list[i] = slist[i];
	c_ptr->num_special = ns;
	for (i = 0; i < ns; i++) c_ptr->special[i] = sspec[i];
	c_ptr->score = score;
}

/*
 * Recursively enumerate subsets of the offered list of a given size and
 * score each as a candidate.
 */
static void enum_subsets(game *g, int who, int type, int offered[], int num,
                         int size, int start, int chosen[], int n_chosen,
                         int special[], int ns, int arg1, int arg2, int arg3)
{
	int i;

	/* Check for completed subset */
	if (n_chosen == size)
	{
		add_candidate(g, who, type, chosen, n_chosen, special, ns,
		              arg1, arg2, arg3);
		return;
	}

	/* Check for room */
	if (num_cand >= MAX_CANDIDATES) return;

	/* Try each remaining card */
	for (i = start; i < num; i++)
	{
		chosen[n_chosen] = offered[i];
		enum_subsets(g, who, type, offered, num, size, i + 1, chosen,
		             n_chosen + 1, special, ns, arg1, arg2, arg3);
	}
}

/*
 * Compute binomial coefficient (capped).
 */
static int binom(int n, int k)
{
	double r = 1;
	int i;

	for (i = 0; i < k; i++) r = r * (n - i) / (i + 1);

	return r > 1e9 ? 1000000000 : (int)r;
}

/*
 * Enumerate and score candidates for a decision, depending on type.
 *
 * answer_list/answer_ns hold the actual translated answer (used as a
 * template for some types and to limit enumeration).
 */
static void enumerate_candidates(game *g, int who, int type, int list[],
                                 int *nl, int special[], int *ns,
                                 int arg1, int arg2, int arg3,
                                 int ans_list[], int ans_nl,
                                 int ans_special[], int ans_ns, int ans_rv)
{
	int chosen[MAX_DECK];
	int sub[MAX_DECK];
	int one[2];
	int i, j, size;

	/* Assume not exhaustive; each subset branch sets this true when it
	 * enumerates the played answer's own size (so the played answer is
	 * guaranteed to have been produced). */
	enum_exhaustive = 0;

	switch (type)
	{
		/* Handled by instrumented AI handlers */
		case CHOICE_ACTION:
		case CHOICE_PLACE:
		{
			game scratch;
			int l_copy[MAX_DECK], s_copy[MAX_DECK];
			int nl_copy = 0, ns_copy = 0;

			/* Copy game and inputs (handlers may modify both) */
			scratch = *g;
			if (nl) { nl_copy = *nl; }
			for (i = 0; i < nl_copy; i++) l_copy[i] = list[i];
			if (ns) { ns_copy = *ns; }
			for (i = 0; i < ns_copy; i++) s_copy[i] = special[i];

			/* Install hooks and run AI handler.  For ACTION,
			 * also capture the opponent prediction the AI uses. */
			ai_option_hook = collect_hook;
			if (type == CHOICE_ACTION)
			{
				num_pred = 0;
				ai_predict_hook = collect_predict;
			}
			ai_func.make_choice(&scratch, who, type, l_copy,
			                    nl ? &nl_copy : NULL, s_copy,
			                    ns ? &ns_copy : NULL,
			                    arg1, arg2, arg3);
			ai_option_hook = NULL;
			ai_predict_hook = NULL;
			break;
		}

		/* Enumerate discard combinations */
		case CHOICE_DISCARD:

			/* Number of cards to discard */
			size = arg1;

			/* Check for small enough enumeration */
			if (binom(*nl, size) <= 200)
			{
				/* Enumerate all combinations */
				enum_subsets(g, who, type, list, *nl, size, 0,
				             chosen, 0, NULL, 0,
				             arg1, arg2, arg3);

				/* Played answer's size fully enumerated */
				if (size == ans_nl) enum_exhaustive = 1;
			}
			else
			{
				/* Score actual answer */
				add_candidate(g, who, type, ans_list, ans_nl,
				              NULL, 0, arg1, arg2, arg3);

				/* Score single-card swaps of actual answer */
				for (i = 0; i < ans_nl; i++)
				{
					for (j = 0; j < *nl; j++)
					{
						int k, dup = 0;

						/* Skip cards already chosen */
						for (k = 0; k < ans_nl; k++)
							if (ans_list[k] ==
							    list[j]) dup = 1;
						if (dup) continue;

						/* Build swapped set */
						for (k = 0; k < ans_nl; k++)
							sub[k] = ans_list[k];
						sub[i] = list[j];

						add_candidate(g, who, type,
						              sub, ans_nl,
						              NULL, 0, arg1,
						              arg2, arg3);
					}
				}
			}
			break;

		/* Enumerate start worlds with all discard sets */
		case CHOICE_START:

			/* Cards to discard down to 4 */
			size = *nl - 4;
			if (size < 0) size = 0;

			/* Loop over start worlds */
			for (i = 0; i < *ns; i++)
			{
				one[0] = special[i];

				/* Enumerate discard sets for this world */
				if (binom(*nl, size) <= 100)
				{
					enum_subsets(g, who, type, list, *nl,
					             size, 0, chosen, 0,
					             one, 1,
					             arg1, arg2, arg3);

					/* Played answer's size fully
					 * enumerated (for every world) */
					if (size == ans_nl) enum_exhaustive = 1;
				}
				else
				{
					/* Just score actual discards */
					add_candidate(g, who, type, ans_list,
					              ans_nl, one, 1,
					              arg1, arg2, arg3);
				}
			}
			break;

		/* Enumerate payment combinations */
		case CHOICE_PAYMENT:

			/* Try every subset size up to hand offered */
			for (size = 0; size <= *nl && size <= 12; size++)
			{
				/* Skip huge enumerations */
				if (binom(*nl, size) > 150) continue;

				/* Use actual answer's specials */
				enum_subsets(g, who, type, list, *nl, size, 0,
				             chosen, 0, ans_special, ans_ns,
				             arg1, arg2, arg3);

				/* Played answer's size fully enumerated */
				if (size == ans_nl) enum_exhaustive = 1;
			}
			break;

		/* Enumerate single-card options */
		case CHOICE_TRADE:
		case CHOICE_SAVE:
		case CHOICE_WINDFALL:

			for (i = 0; i < *nl; i++)
			{
				one[0] = list[i];
				add_candidate(g, who, type, one, 1, NULL, 0,
				              arg1, arg2, arg3);
			}
			break;

		/* Optional single-card options (none is allowed) */
		case CHOICE_DISCARD_PRESTIGE:

			/* No discard */
			add_candidate(g, who, type, one, 0, NULL, 0,
			              arg1, arg2, arg3);

			for (i = 0; i < *nl; i++)
			{
				one[0] = list[i];
				add_candidate(g, who, type, one, 1, NULL, 0,
				              arg1, arg2, arg3);
			}
			break;

		/* Enumerate power choices (parallel card/power arrays) */
		case CHOICE_CONSUME:
		case CHOICE_PRODUCE:

			for (i = 0; i < *nl; i++)
			{
				int sp[1];

				one[0] = list[i];
				sp[0] = special[i];
				add_candidate(g, who, type, one, 1, sp, 1,
				              arg1, arg2, arg3);
			}
			break;

		/* Enumerate card sets from hand to consume */
		case CHOICE_CONSUME_HAND:

			/* Try every allowed size */
			for (size = 0; size <= *nl; size++)
			{
				if (binom(*nl, size) > 100) continue;

				enum_subsets(g, who, type, list, *nl, size, 0,
				             chosen, 0, NULL, 0,
				             arg1, arg2, arg3);

				/* Played answer's size fully enumerated */
				if (size == ans_nl) enum_exhaustive = 1;
			}
			break;

		/* Enumerate good combinations */
		case CHOICE_GOOD:

			/* Try sizes between min and max */
			for (size = arg1; size <= arg2 && size <= *nl; size++)
			{
				if (binom(*nl, size) > 100) continue;

				enum_subsets(g, who, type, list, *nl, size, 0,
				             chosen, 0, special, *ns,
				             arg1, arg2, arg3);

				/* Played answer's size fully enumerated */
				if (size == ans_nl) enum_exhaustive = 1;
			}
			break;

		/* Enumerate discard/world pairs */
		case CHOICE_DISCARD_PRODUCE:

			/* No discard */
			add_candidate(g, who, type, one, 0, one, 0,
			              arg1, arg2, arg3);

			for (i = 0; i < *nl; i++)
			{
				for (j = 0; j < *ns; j++)
				{
					int sp[1];

					one[0] = list[i];
					sp[0] = special[j];
					add_candidate(g, who, type, one, 1,
					              sp, 1, arg1, arg2,
					              arg3);
				}
			}
			break;

		/* Unsupported: no candidate scores */
		default:
			break;
	}
}

/*
 * Emit a decision event.
 */
static void emit_decision(game *g, int who, int type, int list[], int *nl,
                          int special[], int *ns, int arg1, int arg2,
                          int arg3, int ans_list[], int ans_nl,
                          int ans_special[], int ans_ns, int ans_rv)
{
	int i, j;

	/* Flush pending narration first */
	flush_messages();

	printf("{\"event\":\"decision\",\"seq\":%d,\"player\":%d,"
	       "\"type\":\"%s\",", decision_seq++, who,
	       type < NUM_CHOICE_NAMES ? choice_name[type] : "?");

	/* Offered items (for ACTION the list is an output buffer) */
	printf("\"offered\":[");
	for (i = 0; type != CHOICE_ACTION && nl && i < *nl; i++)
	{
		if (i) putchar(',');
		json_str(card_name(g, list[i]));
	}
	printf("],\"offered_special\":[");
	for (i = 0; ns && i < *ns; i++)
	{
		if (i) putchar(',');
		if (type == CHOICE_CONSUME || type == CHOICE_PRODUCE ||
		    type == CHOICE_GOOD)
			printf("%d", special[i]);
		else
			json_str(card_name(g, special[i]));
	}
	printf("],\"args\":[%d,%d,%d],", arg1, arg2, arg3);

	/* Candidates with scores */
	printf("\"options\":[");
	for (i = 0; i < num_cand; i++)
	{
		if (i) putchar(',');
		printf("{\"list\":[");
		for (j = 0; j < cand[i].num; j++)
		{
			if (j) putchar(',');
			if (type == CHOICE_ACTION)
				printf("%d", cand[i].list[j]);
			else
				json_str(card_name(g, cand[i].list[j]));
		}
		printf("],\"special\":[");
		for (j = 0; j < cand[i].num_special; j++)
		{
			if (j) putchar(',');
			if (type == CHOICE_CONSUME ||
			    type == CHOICE_PRODUCE ||
			    type == CHOICE_GOOD)
				printf("%d", cand[i].special[j]);
			else
				json_str(card_name(g, cand[i].special[j]));
		}
		printf("],\"score\":%.6f}", cand[i].score);
	}
	printf("],");

	/* Predicted opponent action distribution (ACTION decisions) */
	printf("\"predictions\":[");
	for (i = 0; i < num_pred; i++)
	{
		if (i) putchar(',');
		printf("{\"player\":%d,\"actions\":[%d", preds[i].who,
		       preds[i].act0);
		if (preds[i].act1 != -1) printf(",%d", preds[i].act1);
		printf("],\"prob\":%.4f}", preds[i].prob);
	}
	printf("],");

	/* Actual answer */
	printf("\"chosen\":{\"rv\":%d,", ans_rv);

	/* Name the return value for card-valued types */
	if (type == CHOICE_PLACE || type == CHOICE_ANTE ||
	    type == CHOICE_KEEP || type == CHOICE_TAKEOVER)
	{
		printf("\"rv_name\":");
		json_str(card_name(g, ans_rv));
		printf(",");
	}

	printf("\"list\":[");
	for (i = 0; i < ans_nl; i++)
	{
		if (i) putchar(',');
		if (type == CHOICE_ACTION)
			printf("%d", ans_list[i]);
		else
			json_str(card_name(g, ans_list[i]));
	}
	printf("],\"special\":[");
	for (i = 0; i < ans_ns; i++)
	{
		if (i) putchar(',');
		if (type == CHOICE_CONSUME || type == CHOICE_PRODUCE ||
		    type == CHOICE_GOOD)
			printf("%d", ans_special[i]);
		else
			json_str(card_name(g, ans_special[i]));
	}
	printf("]},");

	/* Game state */
	printf("\"state\":");
	emit_state(g);
	printf("}\n");

	fflush(stdout);
}

/*
 * Check an expectation entry against the game state.
 */
static void check_expect(game *g, int who, entry *e_ptr)
{
	int actual = -1, expected;
	const char *what = e_ptr->tokens[0];

	if (!strcmp(what, "handnames"))
	{
		const char *have[MAX_DECK];
		const char *want[MAX_DECK];
		int x, i, j, n_have = 0, n_want;

		for (x = 0; x < g->deck_size; x++)
		{
			if (g->deck[x].where == WHERE_HAND &&
			    g->deck[x].owner == who)
				have[n_have++] = g->deck[x].d_ptr->name;
		}
		n_want = e_ptr->num_tokens - 1;
		for (i = 0; i < n_want; i++) want[i] = e_ptr->tokens[i + 1];

		for (i = 0; i < n_have; i++)
			for (j = i + 1; j < n_have; j++)
				if (strcmp(have[i], have[j]) > 0)
				{
					const char *t = have[i];
					have[i] = have[j];
					have[j] = t;
				}
		for (i = 0; i < n_want; i++)
			for (j = i + 1; j < n_want; j++)
				if (strcmp(want[i], want[j]) > 0)
				{
					const char *t = want[i];
					want[i] = want[j];
					want[j] = t;
				}

		if (n_have != n_want)
		{
			fprintf(stderr, "analyzer: hand contents mismatch at "
			        "script line %d for player %d: expected %d "
			        "cards, actual %d\n", e_ptr->line,
			        orig_of[who], n_want, n_have);
			exit(2);
		}

		for (i = 0; i < n_have; i++)
		{
			if (!strcmp(have[i], want[i])) continue;
			fprintf(stderr, "analyzer: hand contents mismatch at "
			        "script line %d for player %d:\n"
			        "  expected: %s\n  actual:   %s\n",
			        e_ptr->line, orig_of[who], want[i], have[i]);
			exit(2);
		}
		return;
	}

	expected = atoi(e_ptr->tokens[1]);

	if (!strcmp(what, "vp")) actual = g->p[who].vp;
	else if (!strcmp(what, "goods"))
	{
		int x;
		actual = 0;
		for (x = 0; x < g->deck_size; x++)
		{
			if (g->deck[x].where == WHERE_ACTIVE &&
			    g->deck[x].owner == who)
				actual += g->deck[x].num_goods;
		}
	}
	else if (!strcmp(what, "goodsdist"))
	{
		/* Compare sorted list of worlds bearing goods */
		char have[2048] = "";
		const char *names[64];
		int x, j, k, n = 0;

		for (x = 0; x < g->deck_size; x++)
		{
			if (g->deck[x].where != WHERE_ACTIVE ||
			    g->deck[x].owner != who) continue;
			for (j = 0; j < g->deck[x].num_goods && n < 64; j++)
				names[n++] = g->deck[x].d_ptr->name;
		}
		for (j = 0; j < n; j++)
			for (k = j + 1; k < n; k++)
				if (strcmp(names[j], names[k]) > 0)
				{
					const char *t = names[j];
					names[j] = names[k];
					names[k] = t;
				}
		for (j = 0; j < n; j++)
		{
			if (j) strcat(have, "|");
			strcat(have, names[j]);
		}
		if (strcmp(have, e_ptr->tokens[1]))
		{
			fprintf(stderr, "analyzer: goods distribution "
			        "mismatch at script line %d for player %d:\n"
			        "  expected: %s\n  actual:   %s\n",
			        e_ptr->line, orig_of[who],
			        e_ptr->tokens[1], have);
			exit(2);
		}
		return;
	}
	else if (!strcmp(what, "hand"))
		actual = count_player_area(g, who, WHERE_HAND);
	else if (!strcmp(what, "tableau"))
		actual = count_player_area(g, who, WHERE_ACTIVE);
	else if (!strcmp(what, "prestige")) actual = g->p[who].prestige;

	if (actual != expected)
	{
		printf("{\"event\":\"mismatch\",\"player\":%d,\"what\":",
		       orig_of[who]);
		json_str(what);
		printf(",\"expected\":%d,\"actual\":%d,\"line\":%d}\n",
		       expected, actual, e_ptr->line);
		fflush(stdout);

		fprintf(stderr, "analyzer: expectation failed at script "
		        "line %d: player %d %s expected %d, actual %d\n",
		        e_ptr->line, orig_of[who], what, expected, actual);
		exit(2);
	}
}

/*
 * Fail translation: in lenient mode flag and return, else die.
 */
#define T_FAIL(fmt, arg, line) do { \
	if (lenient_resolve) { resolve_failed = 1; return; } \
	die(fmt, arg, line); } while (0)

/*
 * Translate a scripted answer into engine values.
 */
static void translate_answer(game *g, int who, entry *e_ptr, int type,
                             int list[], int *nl, int special[], int *ns,
                             int ans_list[], int *ans_nl, int ans_special[],
                             int *ans_ns, int *ans_rv)
{
	int i, x, n_list, n_special;
	char **toks = e_ptr->tokens;
	int num = e_ptr->num_tokens;
	int sp_at = e_ptr->special_at >= 0 ? e_ptr->special_at : num;

	*ans_nl = 0;
	*ans_ns = 0;
	*ans_rv = 0;

	/* Reset duplicate-name tracking */
	reset_used();

	switch (type)
	{
		/* Actions: numeric codes */
		case CHOICE_ACTION:

			ans_list[0] = ans_list[1] = -1;
			for (i = 0; i < num && i < 2; i++)
				ans_list[i] = atoi(toks[i]);
			*ans_nl = 2;
			break;

		/* Single-card answers via return value */
		case CHOICE_PLACE:
		case CHOICE_ANTE:
		case CHOICE_KEEP:

			*ans_rv = resolve_card(g, toks[0], list, nl ? *nl : 0);
			break;

		/* Numeric return value */
		case CHOICE_LUCKY:
		case CHOICE_SEARCH_TYPE:
		case CHOICE_SEARCH_KEEP:
		case CHOICE_OORT_KIND:
		case CHOICE_TAKEOVER:

			*ans_rv = atoi(toks[0]);
			break;

		/* Goods: list of cards, specials echoed from question */
		case CHOICE_GOOD:

			for (i = 0; i < num; i++)
			{
				x = resolve_card(g, toks[i], list,
				                 nl ? *nl : 0);
				if (x < 0) T_FAIL("cannot resolve card \"%s\"",
				               toks[i], e_ptr->line);
				ans_list[(*ans_nl)++] = x;
			}

			/* Echo question's special items */
			for (i = 0; ns && i < *ns; i++)
				ans_special[(*ans_ns)++] = special[i];
			break;

		/* Card list answers */
		case CHOICE_DISCARD:
		case CHOICE_SAVE:
		case CHOICE_DISCARD_PRESTIGE:
		case CHOICE_CONSUME_HAND:
		case CHOICE_TRADE:
		case CHOICE_WINDFALL:
		case CHOICE_PAYMENT:
		case CHOICE_START:
		case CHOICE_DISCARD_PRODUCE:
		case CHOICE_UPGRADE:

			/* Check for explicit "none" */
			if (num == 1 && !strcmp(toks[0], "none")) break;

			/* Translate list part */
			for (i = 0; i < sp_at; i++)
			{
				x = resolve_card(g, toks[i], list,
				                 nl ? *nl : 0);
				if (x < 0 && strcmp(toks[i], "none"))
					T_FAIL("cannot resolve card \"%s\"",
					    toks[i], e_ptr->line);
				if (x >= 0) ans_list[(*ans_nl)++] = x;
			}

			/* Translate special part (separate offered array,
			 * so reset position tracking) */
			reset_used();
			for (i = sp_at; i < num; i++)
			{
				x = resolve_card(g, toks[i], special,
				                 ns ? *ns : 0);
				if (x < 0)
					T_FAIL("cannot resolve special \"%s\"",
					    toks[i], e_ptr->line);
				ans_special[(*ans_ns)++] = x;
			}
			break;

		/* Power choices: card name + power index */
		case CHOICE_CONSUME:
		case CHOICE_PRODUCE:
		case CHOICE_SETTLE:

			/* Check for "none" (declined power) */
			if (!strcmp(toks[0], "none")) break;

			/* Check for Prestige Consume-Trade bonus power */
			if (!strcmp(toks[0], "prestige"))
			{
				ans_list[0] = -1;
				ans_special[0] = -1;
				*ans_nl = 1;
				*ans_ns = 1;
				break;
			}

			/* Resolve card */
			x = resolve_card(g, toks[0], list, nl ? *nl : 0);
			if (x < 0) T_FAIL("cannot resolve card \"%s\"",
			               toks[0], e_ptr->line);

			/* Find matching pair in offered arrays */
			n_list = nl ? *nl : 0;
			n_special = num >= 2 ? atoi(toks[1]) : -1;

			for (i = 0; i < n_list; i++)
			{
				if (list[i] != x) continue;
				if (n_special >= 0 &&
				    special[i] != n_special) continue;

				ans_list[0] = list[i];
				ans_special[0] = special[i];
				*ans_nl = 1;
				*ans_ns = 1;
				break;
			}

			if (!*ans_nl) T_FAIL("cannot match power on \"%s\"",
			                  toks[0], e_ptr->line);
			break;

		/* Unsupported types */
		default:
			T_FAIL("unsupported choice type %s in script",
			    type < NUM_CHOICE_NAMES ?
			    choice_name[type] : "?", e_ptr->line);
	}
}

/*
 * Track player rotation so script streams stay attached to the right
 * players.  The engine calls this for every player after each rotation;
 * we shift our mapping once, on the first call.
 */
static void an_notify_rotation(game *g, int who)
{
	int tmp, i;

	/* Only act on first notification of the batch */
	if (who != 0) return;

	/* Rotate mapping */
	tmp = orig_of[0];
	for (i = 0; i < g->num_players - 1; i++) orig_of[i] = orig_of[i + 1];
	orig_of[i] = tmp;
}

/*
 * Make a choice for a scripted player.
 */
static void an_make_choice(game *g, int who, int type, int list[], int *nl,
                           int special[], int *ns, int arg1, int arg2,
                           int arg3)
{
	player *p_ptr = &g->p[who];
	entry *e_ptr;
	int ans_list[MAX_DECK], ans_nl;
	int ans_special[MAX_DECK], ans_ns;
	int ans_rv;
	int *l_ptr;
	int i, op;
	int translated = 0;
	char msg[1024];

	/* Get original script index for this seat */
	op = orig_of[who];

	/* Trace asks in verbose mode */
	if (verbose)
	{
		fprintf(stderr, "[ask seat=%d orig=%d name=%s type=%s nl=%d:",
		        who, op, g->p[who].name,
		        type < NUM_CHOICE_NAMES ? choice_name[type] : "?",
		        nl ? *nl : -1);
		if (type != CHOICE_ACTION && nl)
			for (i = 0; i < *nl; i++)
				fprintf(stderr, " %s",
				        card_name(g, list[i]));
		fprintf(stderr, "]\n");
	}

	/* Scan for the next usable entry.  Entries carry a consumed
	 * flag so that same-type optional entries passed over by one ask
	 * (e.g. a windfall whose world is busy right now) remain
	 * available for later asks of that type.  Skips of other
	 * non-matching optionals and held expects only commit when a
	 * matching entry is found. */
	{
		int scan;
		int held[64], num_held = 0;
		int tentative[64], num_tent = 0;
		int found = 0;
		int passed_mandatory = 0;

		e_ptr = NULL;

		for (scan = script_pos[op]; scan < script_len[op]; scan++)
		{
			entry *s_ptr = &script[op][scan];

			if (s_ptr->consumed) continue;

			/* Hold expects */
			if (s_ptr->kind == ENTRY_EXPECT)
			{
				if (num_held < 64) held[num_held++] = scan;
				continue;
			}

			/* Matching optional entries must also resolve */
			if (s_ptr->type == type && s_ptr->optional)
			{
				int dummy;
				(void)dummy;
				lenient_resolve = 1;
				resolve_failed = 0;
				translate_answer(g, who, s_ptr, type, list,
				                 nl, special, ns, ans_list,
				                 &ans_nl, ans_special, &ans_ns,
				                 &ans_rv);
				lenient_resolve = 0;

				/* Windfall goods of the same type are
				 * fungible: substitute an offered world of
				 * the entry's good type when the named
				 * world is unavailable */
				if (resolve_failed && type == CHOICE_WINDFALL
				    && nl && s_ptr->num_tokens > 0)
				{
					design *d_ptr = design_by_name(
						s_ptr->tokens[0]);
					int k;

					for (k = 0; d_ptr && k < *nl; k++)
					{
						if (g->deck[list[k]].d_ptr->
						        good_type !=
						    d_ptr->good_type)
							continue;
						ans_list[0] = list[k];
						ans_nl = 1;
						ans_ns = 0;
						ans_rv = 0;
						resolve_failed = 0;
						if (verbose)
							fprintf(stderr,
							    "[windfall subst"
							    " %s -> %s]\n",
							    s_ptr->tokens[0],
							    g->deck[list[k]].
							    d_ptr->name);
						break;
					}
				}

				if (resolve_failed)
				{
					/* Leave for a later ask of this
					 * type */
					if (verbose)
						fprintf(stderr, "[defer "
						        "optional line %d]\n",
						        s_ptr->line);
					continue;
				}
				e_ptr = s_ptr;
				translated = 1;
				found = 1;
				break;
			}

			/* Matching PLACE entries must also resolve (a
			 * settle placement scripted while the engine asks
			 * for the develop phase stays put) */
			if (s_ptr->type == type && type == CHOICE_PLACE)
			{
				lenient_resolve = 1;
				resolve_failed = 0;
				translate_answer(g, who, s_ptr, type, list,
				                 nl, special, ns, ans_list,
				                 &ans_nl, ans_special, &ans_ns,
				                 &ans_rv);
				lenient_resolve = 0;
				if (!resolve_failed &&
				    (ans_rv != -1 ||
				     !strcmp(s_ptr->tokens[0], "none")))
				{
					e_ptr = s_ptr;
					translated = 1;
					found = 1;
				}
				break;
			}

			/* Matching mandatory entry (only in order) */
			if (s_ptr->type == type)
			{
				if (passed_mandatory) break;
				e_ptr = s_ptr;
				found = 1;
				break;
			}

			/* Non-matching optional: skip tentatively */
			if (s_ptr->optional)
			{
				if (num_tent < 64)
					tentative[num_tent++] = scan;
				continue;
			}

			/* Round fence: never scan past an ACTION entry */
			if (s_ptr->type == CHOICE_ACTION) break;

			/* Non-matching mandatory: pass without consuming
			 * (game logs sometimes record an effect later in
			 * the move than the engine asks for it); only
			 * optional entries may match beyond this point */
			passed_mandatory = 1;
		}

		if (found)
		{
			/* Commit: consume matched entry, different-type
			 * optionals passed over (only those before any
			 * passed mandatory entry), and held expects */
			e_ptr->consumed = 1;
			for (i = 0; i < num_tent; i++)
			{
				if (passed_mandatory) break;
				script[op][tentative[i]].consumed = 1;
			}
			for (i = 0; i < num_held; i++)
			{
				script[op][held[i]].consumed = 1;
				if (!strcmp(script[op][held[i]].tokens[0],
				            "handnames"))
					check_expect(g, who,
					             &script[op][held[i]]);
				else if (num_pending[op] < 64)
					pending_expect[op][num_pending[op]++] =
						&script[op][held[i]];
			}

			/* Advance past leading consumed entries */
			while (script_pos[op] < script_len[op] &&
			       script[op][script_pos[op]].consumed)
				script_pos[op]++;
		}
		else
		{
			e_ptr = NULL;
		}
	}

	/* No matching entry: some asks are declinable (the engine asks
	 * even when the player passes or uses zero cards) */
	if (!e_ptr && !translated)
	{
		/* Find the next unconsumed choice entry (for messages and
		 * exhaustion checks) */
		entry *u_ptr = NULL;
		for (i = script_pos[op]; i < script_len[op]; i++)
		{
			if (script[op][i].consumed) continue;
			if (script[op][i].kind != ENTRY_CHOICE) continue;
			u_ptr = &script[op][i];
			break;
		}

		/* Conceded game: the log simply stops, possibly in the
		 * middle of a round.  If this player's script is exhausted,
		 * replay cannot safely answer the current ask; BGA may still
		 * have logged another player's choices before the concession,
		 * but engine ask order can make those unreachable. */
		if (concede_game && !u_ptr)
		{
			flush_messages();
			printf("{\"event\":\"log\",\"text\":"
			       "\"Game conceded here.\"}\n");
			g->game_over = 1;
			conceded_here = 1;
			ans_nl = 0;
			ans_ns = 0;
			ans_rv = 0;
			goto write_answer;
		}

		/* GOOD asks for a power the player skipped: mark the
		 * power used so the engine's consume loop moves past it
		 * (BGA allows leaving consume powers unused) */
		if (type == CHOICE_GOOD && ns && *ns >= 2 &&
		    special[0] >= 0 && special[0] < g->deck_size)
		{
			g->deck[special[0]].misc |=
				1 << (MISC_USED_SHIFT + special[1]);
			ans_nl = 0;
			ans_ns = 0;
			ans_rv = 0;
			goto write_answer;
		}

		/* Compatibility fallback for older scripts that did not
		 * emit explicit SETTLE power choices from BGA state ids.
		 * Accept the power when the script has another placement
		 * pending this round (the upcoming PLACE ask will consume
		 * it); decline otherwise.  With a doubled settle either
		 * choice reaches the same round-end state, since the
		 * extra PLACE ask comes from one phase or the other. */
		if (type == CHOICE_SETTLE)
		{
			entry *n_ptr = NULL;

			for (i = script_pos[op]; i < script_len[op]; i++)
			{
				if (script[op][i].consumed) continue;
				if (script[op][i].kind != ENTRY_CHOICE)
					continue;

				/* Skip optional non-placement entries */
				if (script[op][i].optional &&
				    script[op][i].type != CHOICE_PLACE)
					continue;

				n_ptr = &script[op][i];
				break;
			}

			if (n_ptr && n_ptr->type == CHOICE_PLACE &&
			    strcmp(n_ptr->tokens[0], "none") != 0)
			{
				/* Use the first offered power */
				ans_list[0] = list[0];
				ans_special[0] = special[0];
				ans_nl = 1;
				ans_ns = 1;
			}
			else
			{
				/* Decline */
				ans_nl = 0;
				ans_ns = 0;
			}
			ans_rv = 0;
			goto write_answer;
		}

		if (type != CHOICE_PLACE &&
		    type != CHOICE_CONSUME &&
		    type != CHOICE_CONSUME_HAND &&
		    type != CHOICE_DISCARD_PRESTIGE &&
		    type != CHOICE_TAKEOVER &&
		    type != CHOICE_WINDFALL)
		{
			sprintf(msg, "script mismatch for player %d: engine "
			        "asks %s, script has %s", op,
			        type < NUM_CHOICE_NAMES ?
			        choice_name[type] : "?",
			        u_ptr && u_ptr->type < NUM_CHOICE_NAMES ?
			        choice_name[u_ptr->type] : "(end)");
			die("%s", msg, u_ptr ? u_ptr->line : 0);
		}
	}

	/* Evaluate held expects at round boundaries */
	if (type == CHOICE_ACTION)
	{
		for (i = 0; i < num_pending[op]; i++)
			check_expect(g, who, pending_expect[op][i]);
		num_pending[op] = 0;
	}

	/* Trace consumed entry in verbose mode */
	if (verbose)
		fprintf(stderr, "[entry line %d%s]\n",
		        e_ptr ? e_ptr->line : -1,
		        translated ? " lenient" : "");

	/* Translate answer (NULL entry: declined placement) */
	if (!e_ptr)
	{
		ans_nl = 0;
		ans_ns = 0;
		ans_rv = -1;
	}
	else if (!translated)
	{
		translate_answer(g, who, e_ptr, type, list, nl, special, ns,
		                 ans_list, &ans_nl, ans_special, &ans_ns,
		                 &ans_rv);
	}

	/* Canonicalize the finalized answer: these answer lists are sets
	 * of cards, so keep them sorted from here on -- candidates are
	 * stored sorted too, letting every later comparison and the UI's
	 * match be a plain ordered one.  ACTION lists are action codes
	 * whose order is handled separately, so leave those alone. */
	if (type != CHOICE_ACTION)
	{
		sort_ints(ans_list, ans_nl);
		sort_ints(ans_special, ans_ns);
	}

	/* Score and emit decision for review player */
	if (op == review_player)
	{
		static game eval_base;
		game *eg = g;

		/* Reset candidates and predictions */
		num_cand = 0;
		num_pred = 0;

		/* Compute option scores */
		if (score_options)
		{
			enumerate_candidates(eg, who, type, list, nl, special,
			                     ns, arg1, arg2, arg3, ans_list,
			                     ans_nl, ans_special, ans_ns,
			                     ans_rv);

			/* If enumeration exhaustively covered the played
			 * answer's size, the played answer itself must be
			 * among the candidates.  If it isn't, enumeration or
			 * the candidate comparison is broken -- fail loudly
			 * rather than silently emitting a duplicate or a
			 * phantom option. */
			if (enum_exhaustive &&
			    !answer_present(ans_list, ans_nl, ans_special,
			                    ans_ns))
			{
				die("played %s answer missing from exhaustive "
				    "enumeration", choice_name[type], 0);
			}

			/* Score the actual answer too if not covered */
			if (type != CHOICE_ACTION && type != CHOICE_PLACE)
			{
				add_candidate(eg, who, type, ans_list, ans_nl,
				              ans_special, ans_ns, arg1,
				              arg2, arg3);
			}
		}

		/* Emit decision event (reported as original player index) */
		emit_decision(g, op, type, list, nl, special, ns, arg1,
		              arg2, arg3, ans_list, ans_nl, ans_special,
		              ans_ns, ans_rv);
	}

	/* For PLACE-like types the answer list is the rv */
	if (type == CHOICE_PLACE || type == CHOICE_ANTE ||
	    type == CHOICE_KEEP || type == CHOICE_LUCKY ||
	    type == CHOICE_SEARCH_TYPE || type == CHOICE_SEARCH_KEEP ||
	    type == CHOICE_OORT_KIND || type == CHOICE_TAKEOVER)
	{
		ans_nl = 0;
		ans_ns = 0;
	}

	/* Write answer to choice log */
write_answer:
	l_ptr = &p_ptr->choice_log[p_ptr->choice_size];

	*l_ptr++ = type;
	*l_ptr++ = ans_rv;

	*l_ptr++ = ans_nl;
	for (i = 0; i < ans_nl; i++) *l_ptr++ = ans_list[i];

	*l_ptr++ = ans_ns;
	for (i = 0; i < ans_ns; i++) *l_ptr++ = ans_special[i];

	p_ptr->choice_size = l_ptr - p_ptr->choice_log;
}

/*
 * Player control interface for scripted players.
 */
static decisions script_func =
{
	NULL,
	an_notify_rotation,
	NULL,
	an_make_choice,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
};

/*
 * Main entry point.
 */
int main(int argc, char *argv[])
{
	int i, j;
	char *script_path = NULL;

	/* Parse arguments */
	for (i = 1; i < argc; i++)
	{
		if (!strcmp(argv[i], "-v")) verbose = 1;
		else if (!strcmp(argv[i], "--no-score")) score_options = 0;
		else script_path = argv[i];
	}

	if (!script_path)
	{
		fprintf(stderr, "usage: analyzer [-v] [--no-score] "
		        "<script>\n");
		return 1;
	}

	/* Read card database */
	if (read_cards(NULL) < 0)
	{
		fprintf(stderr, "analyzer: cannot read cards.txt\n");
		return 1;
	}

	/* No final score checks by default */
	for (i = 0; i < MAX_PLAYER; i++) final_score[i] = -1;

	/* Load script */
	load_script(script_path);

	/* Build campaign from scripted draws */
	memset(&analyzer_camp, 0, sizeof(analyzer_camp));
	analyzer_camp.name = "analyzer";
	analyzer_camp.desc = "scripted replay";
	analyzer_camp.expanded = real_game.expanded;
	analyzer_camp.min_num_players = real_game.num_players;
	analyzer_camp.max_num_players = real_game.num_players;
	analyzer_camp.advanced = real_game.advanced;
	analyzer_camp.goal_disabled = real_game.goal_disabled;
	analyzer_camp.takeover_disabled = real_game.takeover_disabled;

	for (i = 0; i < real_game.num_players; i++)
	{
		for (j = 0; j < draw_len[i]; j++)
		{
			analyzer_camp.order[i][j] = draw_script[i][j];
		}
		analyzer_camp.size[i] = draw_len[i];
	}

	/* Scripted start world options */
	for (i = 0; i < real_game.num_players; i++)
	{
		analyzer_camp.start_choice[i][0] = start_options[i][0];
		analyzer_camp.start_choice[i][1] = start_options[i][1];
	}

	/* Forced goals */
	for (i = 0; i < num_forced_goal; i++)
	{
		analyzer_camp.goal[i] = forced_goal[i];
	}
	analyzer_camp.num_goal = num_forced_goal;

	real_game.camp = &analyzer_camp;

	/* Set up players */
	for (i = 0; i < real_game.num_players; i++)
	{
		/* Initialize rotation mapping */
		orig_of[i] = i;

		if (!real_game.p[i].name)
		{
			char buf[64];
			sprintf(buf, "Player %d", i);
			real_game.p[i].name = strdup(buf);
		}

		real_game.p[i].control = &script_func;
		real_game.p[i].choice_log =
			(int *)malloc(sizeof(int) * 4096);
		real_game.p[i].choice_size = 0;
		real_game.p[i].choice_pos = 0;
	}

	/* Load AI networks (learning disabled) */
	ai_func.init(&real_game, 0, 0.0);

	/* Initialize and play game */
	init_game(&real_game);
	begin_game(&real_game);
	while (game_round(&real_game));
	score_game(&real_game);
	declare_winner(&real_game);

	/* Flush remaining narration */
	flush_messages();

	/* Evaluate any remaining expectations (final state checks).
	 * A conceded replay may intentionally stop before later script
	 * lines, so remaining post-concession expectations are not valid
	 * checks against the stopped engine state. */
	for (i = 0; !conceded_here && i < real_game.num_players; i++)
	{
		int o = orig_of[i];
		int j;

		for (j = 0; j < num_pending[o]; j++)
			check_expect(&real_game, i, pending_expect[o][j]);
		num_pending[o] = 0;

		for (j = script_pos[o]; j < script_len[o]; j++)
		{
			if (script[o][j].consumed) continue;
			if (script[o][j].kind == ENTRY_EXPECT)
				check_expect(&real_game, i, &script[o][j]);
		}
	}

	/* Verify final scores against the source game */
	for (i = 0; i < real_game.num_players; i++)
	{
		int o = orig_of[i];

		if (final_score[o] < 0) continue;
		if (real_game.p[i].end_vp != final_score[o])
		{
			fprintf(stderr, "analyzer: final score mismatch for "
			        "player %d (%s): engine %d, source %d\n",
			        o, real_game.p[i].name,
			        real_game.p[i].end_vp, final_score[o]);
			fflush(stdout);
			exit(2);
		}
	}

	/* Emit final result */
	printf("{\"event\":\"result\",\"players\":[");
	for (i = 0; i < real_game.num_players; i++)
	{
		if (i) putchar(',');
		printf("{\"name\":");
		json_str(real_game.p[i].name);
		printf(",\"vp\":%d,\"winner\":%d,"
		       "\"chips\":%d,\"goal_vp\":%d,\"prestige\":%d,"
		       "\"card_vp\":%d}",
		       real_game.p[i].end_vp, real_game.p[i].winner,
		       real_game.p[i].vp, real_game.p[i].goal_vp,
		       real_game.p[i].prestige,
		       real_game.p[i].end_vp - real_game.p[i].vp -
		       real_game.p[i].goal_vp - real_game.p[i].prestige);
	}
	printf("]}\n");

	return 0;
}
