#ifndef RFTG_TRACE_H
#define RFTG_TRACE_H

struct game;

typedef enum trace_decision_scope
{
	TRACE_SCOPE_REVIEW,
	TRACE_SCOPE_ALL
} trace_decision_scope;

typedef enum trace_decision_status
{
	TRACE_DECISION_ANSWERED,
	TRACE_DECISION_PENDING
} trace_decision_status;

typedef struct trace_context
{
	const int *original_player_by_seat;
	int decision_sequence;
} trace_context;

void trace_emit_header(trace_decision_scope scope);
void trace_emit_json_string(const char *value);
void trace_emit_int_array(const int values[], int count);
void trace_emit_state_digest(struct game *g,
                             const int original_player_by_seat[]);

/* Emits the canonical fields but deliberately leaves the object open so
 * analyzer presentation fields can be appended before trace_end_decision. */
void trace_begin_decision(trace_context *trace, struct game *g, int player,
                          const char *choice_type, int list[], int num_list,
                          int special[], int num_special, int arg1, int arg2,
                          int arg3, int answer_rv, int answer_list[],
                          int num_answer_list, int answer_special[],
                          int num_answer_special,
                          trace_decision_status status);
void trace_end_decision(void);

void trace_emit_draw(struct game *g, const int original_player_by_seat[],
                     int who, int card);
void trace_emit_good(struct game *g, int world, int card);
void trace_emit_refresh(struct game *g);
void trace_emit_start_options(int player, int red, int blue);
void trace_emit_result(struct game *g, const int original_player_by_seat[]);

#endif
