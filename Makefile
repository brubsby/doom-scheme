# Top-level tasks. Run inside `nix develop`.

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

run:
	scheme --libdirs src --script src/main.ss

clean:
	rm -rf build

.PHONY: all wad oracle oracle-traces run clean FORCE
