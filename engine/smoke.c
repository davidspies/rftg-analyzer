#include "rftg.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void display_error(char *msg) { fprintf(stderr, "%s", msg); }
void message_add(game *g, char *msg) { }
void message_add_formatted(game *g, char *msg, char *tag) { }
int game_rand(game *g) { return simple_rand(&g->random_seed); }

int main(void)
{
    game g;
    int i;

    if (read_cards(NULL) < 0) { printf("cards fail\n"); return 1; }
    memset(&g, 0, sizeof(game));
    g.num_players = 2;
    g.expanded = 0;
    g.random_seed = 42;
    for (i = 0; i < 2; i++) {
        g.p[i].control = &ai_func;
        g.p[i].name = i ? "Bob" : "Alice";
        ai_func.init(&g, i, 0.0);
        g.p[i].choice_log = (int *)malloc(sizeof(int) * 4096);
        g.p[i].choice_size = 0;
        g.p[i].choice_pos = 0;
    }
    init_game(&g);
    begin_game(&g);
    while (game_round(&g));
    score_game(&g);
    declare_winner(&g);
    for (i = 0; i < 2; i++)
        printf("%s: %d VP %s\n", g.p[i].name, g.p[i].end_vp,
               g.p[i].winner ? "(winner)" : "");
    return 0;
}
