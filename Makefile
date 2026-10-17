# Top-level tasks. Run inside `nix develop`.

# Libraries compile to src/doom/*.so on first use and recompile when they
# (or files they include) change.
SCHEME := scheme --libdirs src --compile-imported-libraries

TRACES := $(foreach d,demo1 demo2 demo3,build/oracle/$(d).trace)

all: oracle-traces

wad: wads/doom1.wad
wads/doom1.wad:
	tools/fetch-wad.sh

oracle: build/oracle/doom-oracle
build/oracle/doom-oracle: FORCE
	@$(MAKE) -s --no-print-directory -C tools/oracle

oracle-traces: $(TRACES)
build/oracle/%.trace: build/oracle/doom-oracle wads/doom1.wad tools/oracle/run.sh
	tools/oracle/run.sh $* $@

# Reference values from the oracle's probes, then the Scheme test suite.
build/oracle/fixed-cases.txt: build/oracle/doom-oracle
	build/oracle/fixed_probe 100000 > $@
build/oracle/tables.txt: build/oracle/doom-oracle
	build/oracle/tables_probe > $@

# The oracle's first title-screen frame (no -timedemo: D_StartTitle shows TITLEPIC).
build/oracle/title.trace: build/oracle/doom-oracle wads/doom1.wad
	mkdir -p build/oracle/home
	env -i HOME=$(CURDIR)/build/oracle/home DOOMWADDIR=$(CURDIR)/wads \
	  ORACLE_TRACE=$(CURDIR)/$@ ORACLE_MAX_FRAMES=1 \
	  setarch -R build/oracle/doom-oracle > build/oracle/title.log 2>&1

# Per-map structure dumps and automap frames (tools/oracle/levels.sh).
build/oracle/levels/.stamp: build/oracle/doom-oracle wads/doom1.wad tools/oracle/levels.sh
	tools/oracle/levels.sh build/oracle/levels

# Renderer reference frames: cameras from the maps' things, rendered by the
# oracle right after each level loads (tools/oracle/render.sh).
build/oracle/cameras/.stamp: wads/doom1.wad tools/oracle/cameras.py
	python3 -I tools/oracle/cameras.py wads/doom1.wad build/oracle/cameras && touch $@
build/oracle/render/.stamp: build/oracle/doom-oracle build/oracle/cameras/.stamp tools/oracle/render.sh
	tools/oracle/render.sh build/oracle/cameras build/oracle/render

# Synthetic -nomonsters demos and the oracle's traces of them (tests/sim.ss).
build/oracle/demos/.stamp: build/oracle/doom-oracle tools/oracle/demos.py tools/oracle/run.sh
	python3 -I tools/oracle/demos.py build/oracle/demos
	for f in build/oracle/demos/*.lmp; do n=$$(basename $$f .lmp); \
	  DEMODIR=build/oracle/demos tools/oracle/run.sh $$n build/oracle/demos/$$n.trace > /dev/null || exit 1; \
	done
	touch $@

# The title loop, with fake time (tests/loop.ss).
build/oracle/attract.trace: build/oracle/doom-oracle wads/doom1.wad tools/oracle/attract.sh
	tools/oracle/attract.sh $@ 12000

# Regenerate the info/sounds tables from the C (output is committed).
info:
	python3 -I tools/infogen.py reference/doom/linuxdoom-1.10 src/doom

test: wads/doom1.wad build/oracle/fixed-cases.txt build/oracle/tables.txt build/oracle/title.trace \
      build/oracle/levels/.stamp build/oracle/render/.stamp build/oracle/demos/.stamp \
      build/oracle/attract.trace $(TRACES)
	@for t in tests/*.ss; do [ $$t = tests/lib.ss ] && continue; \
	  echo "== $$t"; $(SCHEME) --script $$t || exit 1; done

run:
	$(SCHEME) --script src/main.ss $(ARGS)

clean:
	rm -rf build src/doom/*.so src/doom/*.wpo

.PHONY: all wad oracle oracle-traces info test run clean FORCE
