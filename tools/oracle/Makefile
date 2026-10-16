# Headless linuxdoom-1.10 "oracle": the original C, built 32-bit (the 1993
# code assumes 32-bit pointers), with X11/OSS replaced by null layers and a
# per-tic trace hook. See oracle.c for the trace format.

ROOT    := $(abspath ../..)
SRC     := $(ROOT)/reference/doom/linuxdoom-1.10
B       := $(ROOT)/build/oracle
CC      := $(ORACLE_CC)   # set by flake.nix: a wrapped i686 gcc
CFLAGS  := -O1 -g -std=gnu89 -fcommon -fno-strict-aliasing -w -DNORMALUNIX -DLINUX -I$(B)/src

ORIG    := doomdef doomstat dstrings i_system i_main i_net tables f_finale \
           f_wipe d_main d_net d_items g_game m_menu m_misc m_argv m_bbox \
           m_fixed m_swap m_cheat m_random am_map p_ceilng p_doors p_enemy \
           p_floor p_inter p_lights p_map p_maputl p_mobj p_plats p_pspr p_setup \
           p_sight p_spec p_switch p_telept p_tick p_saveg p_user r_bsp \
           r_data r_draw r_main r_plane r_segs r_sky r_things w_wad wi_stuff \
           v_video st_lib st_stuff hu_stuff hu_lib s_sound z_zone info sounds
OURS    := i_video_null i_sound_null oracle
OBJS    := $(addprefix $(B)/obj/,$(addsuffix .o,$(ORIG) $(OURS)))

all: $(B)/doom-oracle $(B)/fixed_probe $(B)/tables_probe

$(B)/doom-oracle: $(OBJS)
	$(CC) -o $@ $^ -lm

# Probes: small programs linked against the oracle's own objects that dump
# reference values for tests/.
$(B)/fixed_probe: probes/fixed_probe.c $(B)/obj/m_fixed.o
	$(CC) $(CFLAGS) -o $@ $^
$(B)/tables_probe: probes/tables_probe.c $(addprefix $(B)/obj/,tables.o m_random.o v_video.o)
	$(CC) $(CFLAGS) -o $@ $^

# Pristine copy of the reference source + oracle.patch.
$(B)/src/.patched: oracle.patch
	rm -rf $(B)/src && mkdir -p $(B)/src
	cp $(SRC)/*.[ch] $(B)/src/
	patch -s -p1 -d $(B)/src --no-backup-if-mismatch < oracle.patch
	touch $@

$(addprefix $(B)/obj/,$(addsuffix .o,$(ORIG))): $(B)/obj/%.o: $(B)/src/.patched
	@mkdir -p $(B)/obj
	$(CC) $(CFLAGS) -c -o $@ $(B)/src/$*.c

$(addprefix $(B)/obj/,$(addsuffix .o,$(OURS))): $(B)/obj/%.o: %.c $(B)/src/.patched
	@mkdir -p $(B)/obj
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -rf $(B)

.PHONY: all clean
