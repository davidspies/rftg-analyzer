/*
 * Generate an analyzer script from an AI-vs-AI game.
 *
 * Plays a complete game with AI players and records every draw and
 * choice in the analyzer's script format.  Used to test that the
 * analyzer's scripted replay reproduces games exactly.
 *
 * usage: genscript [-p players] [-e expansion] [-a] [-o] [-r seed]
 */

#include "rftg.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Names of choice types (must match analyzer.c) */
static const char *choice_name[] =
{
	"ACTION", "START", "DISCARD", "SAVE", "DISCARD_PRESTIGE",
	"PLACE", "PAYMENT", "SETTLE", "TAKEOVER", "DEFEND",
	"TAKEOVER_PREVENT", "UPGRADE", "TRADE", "CONSUME", "CONSUME_HAND",
	"GOOD", "LUCKY", "ANTE", "KEEP", "WINDFALL", "PRODUCE",
	"DISCARD_PRODUCE", "SEARCH_TYPE", "SEARCH_KEEP", "OORT_KIND"
};

/* Per-player recorded lines, kept in per-player order */
#define MAX_LINES 8192
static char *draw_lines[MAX_PLAYER][MAX_LINES];
static int num_draw_lines[MAX_PLAYER];
static char *choice_lines[MAX_PLAYER][MAX_LINES];
static int num_choice_lines[MAX_PLAYER];

/* Index of the START-converted discard line, for round-0 merging */
static int start_discard_line[MAX_PLAYER];

/* Map from current seat to original player index */
static int orig_of[MAX_PLAYER];

/* The game */
static game my_game;

/* Verbose flag */
static int verbose;

void display_error(char *msg) { fprintf(stderr, "%s", msg); }
void message_add(game *g, char *msg) { if (verbose) fprintf(stderr, "%s", msg); }
void message_add_formatted(game *g, char *msg, char *tag) { message_add(g, msg); }
int game_rand(game *g) { return simple_rand(&g->random_seed); }

/*
 * Record a draw.
 */
static void record_draw(game *g, int who, int which)
{
	char buf[256];
	int op = orig_of[who];

	sprintf(buf, "draw %d \"%s\"", op, g->deck[which].d_ptr->name);
	draw_lines[op][num_draw_lines[op]++] = strdup(buf);
}

/*
 * Track rotation.
 */
static void gen_notify_rotation(game *g, int who)
{
	int tmp, i;

	if (who != 0) return;

	tmp = orig_of[0];
	for (i = 0; i < g->num_players - 1; i++) orig_of[i] = orig_of[i + 1];
	orig_of[i] = tmp;
}

/*
 * Append a card name (quoted) to a buffer.
 */
static void cat_card(char *buf, game *g, int which)
{
	char tmp[128];

	if (which < 0)
	{
		strcat(buf, " none");
		return;
	}

	sprintf(tmp, " \"%s\"", g->deck[which].d_ptr->name);
	strcat(buf, tmp);
}

/*
 * Make a choice using the AI, then record the answer in script form.
 */
static void gen_make_choice(game *g, int who, int type, int list[], int *nl,
                            int special[], int *ns, int arg1, int arg2,
                            int arg3)
{
	player *p_ptr = &g->p[who];
	int op = orig_of[who];
	int old_size = p_ptr->choice_size;
	int *l_ptr;
	int rv, num, nsp, i;
	int a_list[MAX_DECK], a_special[MAX_DECK];
	char buf[4096], tmp[128];

	/* Let the AI choose */
	ai_func.make_choice(g, who, type, list, nl, special, ns,
	                    arg1, arg2, arg3);

	/* Parse the answer just written to the log */
	l_ptr = &p_ptr->choice_log[old_size];
	l_ptr++;            /* type */
	rv = *l_ptr++;
	num = *l_ptr++;
	for (i = 0; i < num; i++) a_list[i] = *l_ptr++;
	nsp = *l_ptr++;
	for (i = 0; i < nsp; i++) a_special[i] = *l_ptr++;

	/* Emit state expectations before each action choice (the
	 * analyzer holds expects until round boundaries, where engine
	 * state and recorded counts are guaranteed to align) */
	if (type == CHOICE_ACTION && g->round > 0)
	{
		sprintf(buf, "expect %d hand %d", op,
		        count_player_area(g, who, WHERE_HAND));
		choice_lines[op][num_choice_lines[op]++] = strdup(buf);
		sprintf(buf, "expect %d vp %d", op, g->p[who].vp);
		choice_lines[op][num_choice_lines[op]++] = strdup(buf);
		sprintf(buf, "expect %d tableau %d", op,
		        count_player_area(g, who, WHERE_ACTIVE));
		choice_lines[op][num_choice_lines[op]++] = strdup(buf);
	}

	/* Format the script line */
	sprintf(buf, "choice %d %s", op, choice_name[type]);

	switch (type)
	{
		case CHOICE_ACTION:
			sprintf(tmp, " %d %d", a_list[0],
			        num > 1 ? a_list[1] : -1);
			strcat(buf, tmp);
			break;

		case CHOICE_START:
			/* The start world is emitted via p->start as the
			 * first scripted draw; record the discards as a
			 * DISCARD choice (this is how the analyzer's
			 * campaign path asks). */
			sprintf(buf, "choice %d DISCARD", op);
			for (i = 0; i < num; i++)
				cat_card(buf, g, a_list[i]);
			if (!num) strcat(buf, " none");

			/* Remember line for round-0 merging */
			start_discard_line[op] = num_choice_lines[op];
			break;

		case CHOICE_DISCARD:
			/* A round-0 discard following a START choice is a
			 * world-specific extra discard (e.g. 3-card start
			 * hands); the analyzer's campaign path asks both
			 * as one discard, so merge into the START line */
			if (g->round == 0 && start_discard_line[op] >= 0 &&
			    start_discard_line[op] ==
			            num_choice_lines[op] - 1)
			{
				char *old = choice_lines[op]
				                        [num_choice_lines[op] - 1];

				strcpy(buf, old);
				free(old);
				for (i = 0; i < num; i++)
					cat_card(buf, g, a_list[i]);
				choice_lines[op][num_choice_lines[op] - 1] =
					strdup(buf);
				return;
			}

			/* Normal discard */
			for (i = 0; i < num; i++)
				cat_card(buf, g, a_list[i]);
			if (!num) strcat(buf, " none");
			break;

		case CHOICE_PLACE:
		case CHOICE_ANTE:
		case CHOICE_KEEP:
			cat_card(buf, g, rv);
			break;

		case CHOICE_LUCKY:
		case CHOICE_SEARCH_TYPE:
		case CHOICE_SEARCH_KEEP:
		case CHOICE_OORT_KIND:
		case CHOICE_TAKEOVER:
			sprintf(tmp, " %d", rv);
			strcat(buf, tmp);
			break;

		case CHOICE_CONSUME:
		case CHOICE_PRODUCE:
		case CHOICE_SETTLE:
			if (!num)
			{
				/* Declined */
				strcat(buf, " none");
				break;
			}
			if (a_list[0] < 0)
			{
				/* Prestige Consume-Trade bonus power */
				strcat(buf, " prestige");
				break;
			}
			cat_card(buf, g, a_list[0]);
			sprintf(tmp, " %d", a_special[0]);
			strcat(buf, tmp);
			break;

		case CHOICE_GOOD:
			/* Specials (c_idx/o_idx) are echoed from the
			 * question by the analyzer; only emit goods */
			for (i = 0; i < num; i++) cat_card(buf, g, a_list[i]);
			break;

		case CHOICE_SAVE:
			/* The AI answers in list[0] without shrinking the
			 * list; only the first item is the answer */
			cat_card(buf, g, num ? a_list[0] : -1);
			break;

		default:
			/* Card list types (DISCARD, PAYMENT, SAVE, ...) */
			for (i = 0; i < num; i++) cat_card(buf, g, a_list[i]);
			if (nsp)
			{
				strcat(buf, " :");
				for (i = 0; i < nsp; i++)
					cat_card(buf, g, a_special[i]);
			}
			if (!num && !nsp) strcat(buf, " none");
			break;
	}

	choice_lines[op][num_choice_lines[op]++] = strdup(buf);
}

static decisions gen_func;

int main(int argc, char *argv[])
{
	int i, j;
	int num_players = 2, expansion = 0, advanced = 0, promo = 0;
	unsigned int seed = 42;
	char buf[64];

	/* Parse arguments */
	for (i = 1; i < argc; i++)
	{
		if (!strcmp(argv[i], "-p")) num_players = atoi(argv[++i]);
		else if (!strcmp(argv[i], "-e")) expansion = atoi(argv[++i]);
		else if (!strcmp(argv[i], "-a")) advanced = 1;
		else if (!strcmp(argv[i], "-o")) promo = 1;
		else if (!strcmp(argv[i], "-r")) seed = atoi(argv[++i]);
		else if (!strcmp(argv[i], "-v")) verbose = 1;
	}

	/* Read card database */
	if (read_cards(NULL) < 0) exit(1);

	/* Set up game */
	memset(&my_game, 0, sizeof(game));
	my_game.num_players = num_players;
	my_game.expanded = expansion;
	my_game.advanced = advanced;
	my_game.promo = promo;
	my_game.random_seed = seed;
	my_game.goal_disabled = 0;
	my_game.takeover_disabled = 0;
	my_game.camp = NULL;

	/* Wrap AI control */
	gen_func = ai_func;
	gen_func.make_choice = gen_make_choice;
	gen_func.notify_rotation = gen_notify_rotation;

	for (i = 0; i < num_players; i++)
	{
		orig_of[i] = i;
		start_discard_line[i] = -1;
		sprintf(buf, "Player %d", i);
		my_game.p[i].name = strdup(buf);
		my_game.p[i].control = &gen_func;
		ai_func.init(&my_game, i, 0.0);
		my_game.p[i].choice_log = (int *)malloc(sizeof(int) * 4096);
		my_game.p[i].choice_size = 0;
		my_game.p[i].choice_pos = 0;
	}

	/* Install draw hook */
	draw_hook = record_draw;

	/* Play game */
	init_game(&my_game);
	begin_game(&my_game);
	while (game_round(&my_game));
	score_game(&my_game);
	declare_winner(&my_game);

	/* Emit script header */
	printf("# generated by genscript: seed %u\n", seed);
	printf("players %d\n", num_players);
	printf("expanded %d\n", expansion);
	printf("advanced %d\n", advanced);
	printf("promo %d\n", promo);
	printf("goals %d\n", !my_game.goal_disabled);
	printf("takeovers %d\n", !my_game.takeover_disabled);
	printf("seed %u\n", seed + 1);
	printf("review 0\n");
	for (i = 0; i < num_players; i++)
	{
		/* Emit original player names (current seat order maps
		 * back through orig_of) */
		for (j = 0; j < num_players; j++)
		{
			if (orig_of[j] == i)
				printf("name %d \"%s\"\n", i,
				       my_game.p[j].name);
		}
	}

	/* Emit active goals */
	for (i = 0; i < MAX_GOAL; i++)
	{
		if (my_game.goal_active[i])
			printf("goal \"%s\"\n", goal_name[i]);
	}

	/* Emit draws, with each player's start world first */
	for (i = 0; i < num_players; i++)
	{
		/* Find current seat of original player i */
		for (j = 0; j < num_players; j++)
		{
			if (orig_of[j] == i)
				printf("draw %d \"%s\"\n", i,
				       my_game.deck[my_game.p[j].start].
				       d_ptr->name);
		}

		for (j = 0; j < num_draw_lines[i]; j++)
			printf("%s\n", draw_lines[i][j]);
	}

	/* Emit choices */
	for (i = 0; i < num_players; i++)
	{
		for (j = 0; j < num_choice_lines[i]; j++)
			printf("%s\n", choice_lines[i][j]);
	}

	/* Emit final expectations */
	for (i = 0; i < num_players; i++)
	{
		for (j = 0; j < num_players; j++)
		{
			if (orig_of[j] != i) continue;
			fprintf(stderr, "result %d \"%s\": %d VP%s\n", i,
			        my_game.p[j].name, my_game.p[j].end_vp,
			        my_game.p[j].winner ? " (winner)" : "");
		}
	}

	return 0;
}
