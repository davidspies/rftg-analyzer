#include "rftg.h"
#include "trace.h"

#include <assert.h>
#include <stdio.h>

static void sort_ints(int values[], int count)
{
	int i, j, value;

	for (i = 1; i < count; i++)
	{
		value = values[i];
		for (j = i - 1; j >= 0 && values[j] > value; j--)
			values[j + 1] = values[j];
		values[j + 1] = value;
	}
}

static int seat_of_original(game *g, const int original_player_by_seat[],
                            int original)
{
	int seat;
	int found = -1;

	for (seat = 0; seat < g->num_players; seat++)
	{
		if (original_player_by_seat[seat] != original) continue;
		assert(found == -1);
		found = seat;
	}
	assert(found >= 0);
	return found;
}

void trace_emit_header(trace_decision_scope scope)
{
	const char *name;

	if (scope == TRACE_SCOPE_REVIEW) name = "review";
	else
	{
		assert(scope == TRACE_SCOPE_ALL);
		name = "all";
	}
	printf("{\"event\":\"header\",\"format\":\"rftg-trace\","
	       "\"version\":1,\"decision_scope\":\"%s\"}\n", name);
	fflush(stdout);
}

void trace_emit_json_string(const char *value)
{
	assert(value != NULL);
	putchar('"');
	for (; *value; value++)
	{
		switch (*value)
		{
			case '"': fputs("\\\"", stdout); break;
			case '\\': fputs("\\\\", stdout); break;
			case '\n': fputs("\\n", stdout); break;
			case '\t': fputs("\\t", stdout); break;
			default:
				if ((unsigned char)*value < 0x20)
					printf("\\u%04x", (unsigned char)*value);
				else
					putchar(*value);
		}
	}
	putchar('"');
}

void trace_emit_int_array(const int values[], int count)
{
	int i;

	assert(count >= 0);
	assert(count == 0 || values != NULL);
	putchar('[');
	for (i = 0; i < count; i++)
	{
		if (i) putchar(',');
		printf("%d", values[i]);
	}
	putchar(']');
}

void trace_emit_state_digest(game *g, const int original_player_by_seat[])
{
	int tmp[MAX_DECK];
	int original, seat, card, good, count;

	assert(g != NULL);
	assert(original_player_by_seat != NULL);
	printf("{\"round\":%d,\"action\":%d,\"pool\":%d,",
	       g->round, g->cur_action, g->vp_pool);

	count = 0;
	for (card = 0; card < g->deck_size; card++)
		if (g->deck[card].where == WHERE_DECK) count++;
	printf("\"deck\":%d,", count);
	count = 0;
	for (card = 0; card < g->deck_size; card++)
		if (g->deck[card].where == WHERE_DISCARD) count++;
	printf("\"discard\":%d,", count);

	for (card = 0; card < MAX_GOAL; card++)
		tmp[card] = g->goal_active[card];
	printf("\"goal_active\":");
	trace_emit_int_array(tmp, MAX_GOAL);
	for (card = 0; card < MAX_GOAL; card++)
		tmp[card] = g->goal_avail[card];
	printf(",\"goal_avail\":");
	trace_emit_int_array(tmp, MAX_GOAL);
	for (card = 0; card < MAX_GOAL; card++)
		tmp[card] = g->goal_most[card];
	printf(",\"goal_most\":");
	trace_emit_int_array(tmp, MAX_GOAL);

	printf(",\"players\":[");
	for (original = 0; original < g->num_players; original++)
	{
		seat = seat_of_original(g, original_player_by_seat, original);
		if (original) putchar(',');
		printf("{\"vp\":%d,\"prestige\":%d,",
		       g->p[seat].vp, g->p[seat].prestige);

		count = 0;
		for (card = 0; card < g->deck_size; card++)
			if (g->deck[card].where == WHERE_HAND &&
			    g->deck[card].owner == seat) tmp[count++] = card;
		sort_ints(tmp, count);
		printf("\"hand\":");
		trace_emit_int_array(tmp, count);

		count = 0;
		for (card = 0; card < g->deck_size; card++)
			if (g->deck[card].where == WHERE_ACTIVE &&
			    g->deck[card].owner == seat) tmp[count++] = card;
		sort_ints(tmp, count);
		printf(",\"tableau\":");
		trace_emit_int_array(tmp, count);

		count = 0;
		for (card = 0; card < g->deck_size; card++)
		{
			if (g->deck[card].where != WHERE_ACTIVE ||
			    g->deck[card].owner != seat) continue;
			for (good = 0; good < g->deck[card].num_goods; good++)
			{
				assert(count < MAX_DECK);
				tmp[count++] = card;
			}
		}
		sort_ints(tmp, count);
		printf(",\"goods\":");
		trace_emit_int_array(tmp, count);

		count = 0;
		for (card = 0; card < g->deck_size; card++)
			if (g->deck[card].where == WHERE_SAVED &&
			    g->deck[card].owner == seat) tmp[count++] = card;
		sort_ints(tmp, count);
		printf(",\"saved\":");
		trace_emit_int_array(tmp, count);

		for (card = 0; card < MAX_GOAL; card++)
			tmp[card] = g->p[seat].goal_claimed[card];
		printf(",\"goal_claimed\":");
		trace_emit_int_array(tmp, MAX_GOAL);
		for (card = 0; card < MAX_GOAL; card++)
			tmp[card] = g->p[seat].goal_progress[card];
		printf(",\"goal_progress\":");
		trace_emit_int_array(tmp, MAX_GOAL);
		putchar('}');
	}
	printf("]}");
}

void trace_begin_decision(trace_context *trace, game *g, int player,
                          const char *choice_type, int list[], int num_list,
                          int special[], int num_special, int arg1, int arg2,
                          int arg3, int answer_rv, int answer_list[],
                          int num_answer_list, int answer_special[],
                          int num_answer_special,
                          trace_decision_status status)
{
	assert(trace != NULL);
	assert(trace->original_player_by_seat != NULL);
	assert(player >= 0 && player < g->num_players);
	assert(choice_type != NULL);
	assert(status == TRACE_DECISION_ANSWERED ||
	       status == TRACE_DECISION_PENDING);
	if (status == TRACE_DECISION_PENDING)
	{
		assert(answer_rv == 0);
		assert(num_answer_list == 0);
		assert(num_answer_special == 0);
	}

	printf("{\"event\":\"decision\",\"seq\":%d,\"player\":%d,"
	       "\"type\":\"%s\",\"need\":%d,\"query\":{\"list\":",
	       trace->decision_sequence++, player, choice_type,
	       status == TRACE_DECISION_PENDING);
	trace_emit_int_array(list, num_list);
	printf(",\"special\":");
	trace_emit_int_array(special, num_special);
	printf(",\"args\":[%d,%d,%d]},\"answer\":{\"rv\":%d,\"list\":",
	       arg1, arg2, arg3, answer_rv);
	trace_emit_int_array(answer_list, num_answer_list);
	printf(",\"special\":");
	trace_emit_int_array(answer_special, num_answer_special);
	printf("},\"state_digest\":");
	trace_emit_state_digest(g, trace->original_player_by_seat);
}

void trace_end_decision(void)
{
	printf("}\n");
	fflush(stdout);
}

void trace_emit_draw(game *g, const int original_player_by_seat[],
                     int who, int card)
{
	assert(g != NULL);
	assert(original_player_by_seat != NULL);
	assert(!g->simulation);
	assert(who >= 0 && who < g->num_players);
	assert(card >= 0 && card < g->deck_size);
	printf("{\"event\":\"draw\",\"player\":%d,\"card\":%d}\n",
	       original_player_by_seat[who], card);
	fflush(stdout);
}

void trace_emit_good(game *g, int world, int card)
{
	assert(g != NULL);
	assert(!g->simulation);
	assert(world >= 0 && world < g->deck_size);
	assert(card >= 0 && card < g->deck_size);
	printf("{\"event\":\"good\",\"world\":%d,\"card\":%d}\n",
	       world, card);
	fflush(stdout);
}

void trace_emit_refresh(game *g)
{
	assert(g != NULL);
	assert(!g->simulation);
	printf("{\"event\":\"refresh\"}\n");
	fflush(stdout);
}

void trace_emit_start_options(int player, int red, int blue)
{
	assert(player >= 0);
	assert(red >= 0);
	assert(blue >= 0);
	printf("{\"event\":\"start_options\",\"player\":%d,"
	       "\"cards\":[%d,%d]}\n", player, red, blue);
	fflush(stdout);
}

void trace_emit_result(game *g, const int original_player_by_seat[])
{
	int original, seat;

	assert(g != NULL);
	assert(original_player_by_seat != NULL);
	printf("{\"event\":\"result\",\"players\":[");
	for (original = 0; original < g->num_players; original++)
	{
		seat = seat_of_original(g, original_player_by_seat, original);
		assert(g->p[seat].name != NULL);
		if (original) putchar(',');
		printf("{\"player\":%d,\"name\":", original);
		trace_emit_json_string(g->p[seat].name);
		printf(",\"vp\":%d,\"winner\":%d,\"chips\":%d,"
		       "\"goal_vp\":%d,\"prestige\":%d,\"card_vp\":%d}",
		       g->p[seat].end_vp,
		       g->p[seat].winner ? 1 : 0, g->p[seat].vp,
		       g->p[seat].goal_vp, g->p[seat].prestige,
		       g->p[seat].end_vp - g->p[seat].vp -
		       g->p[seat].goal_vp - g->p[seat].prestige);
	}
	printf("]}\n");
	fflush(stdout);
}
