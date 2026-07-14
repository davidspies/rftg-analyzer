SRC = ../engine-src/src
DATA = $(abspath data)
CFLAGS = -O2 -w -I$(SRC) -DRFTGDIR='"$(DATA)"'
ENGINE_SRCS = $(SRC)/engine.c $(SRC)/init.c $(SRC)/ai.c $(SRC)/net.c $(SRC)/loadsave.c

all: analyzer genscript $(DATA)/cards.txt $(DATA)/network $(DATA)/campaign.txt

# The engine loads cards.txt and the trained networks from RFTGDIR
# (= $(DATA)) at runtime.  Those live in the engine submodule; link
# them in so a fresh checkout works after `git submodule update`.
# $(DATA) is gitignored, so this must run on every machine.
$(DATA)/cards.txt: $(SRC)/cards.txt
	mkdir -p $(DATA)
	ln -sfn $(abspath $(SRC)/cards.txt) $@

$(DATA)/campaign.txt: $(SRC)/campaign.txt
	mkdir -p $(DATA)
	ln -sfn $(abspath $(SRC)/campaign.txt) $@

$(DATA)/network: $(SRC)/network
	mkdir -p $(DATA)
	ln -sfn $(abspath $(SRC)/network) $@

analyzer: analyzer.c trace.c trace.h $(ENGINE_SRCS) $(SRC)/rftg.h
	gcc $(CFLAGS) -o $@ analyzer.c trace.c $(ENGINE_SRCS) -lm

genscript: genscript.c $(ENGINE_SRCS) $(SRC)/rftg.h
	gcc $(CFLAGS) -o $@ genscript.c $(ENGINE_SRCS) -lm

smoke: smoke.c $(ENGINE_SRCS)
	gcc $(CFLAGS) -o $@ smoke.c $(ENGINE_SRCS) -lm

clean:
	rm -f analyzer genscript smoke
	rm -f $(DATA)/cards.txt $(DATA)/campaign.txt $(DATA)/network

.PHONY: all clean
