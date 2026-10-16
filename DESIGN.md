# doom-scheme — design

A port of id Software's DOOM to Scheme.

**Target: linuxdoom-1.10** (`reference/doom/`), quirks and all. The C code is the
spec. **Chocolate Doom 3.1.1** (`reference/chocolate-doom/`) is the second
reference. It is a well-commented, actively maintained reading of the same
engine, and we consult it for:
- **What an obscure piece of C means**, plus the documented vanilla bugs.
- **Clean SDL2 platform code to crib from** (`i_video.c`, `i_sound.c`, `i_input.c`).
- **Edge cases that linuxdoom-1.10 leaves undefined.** Examples are reading past
  the end of a lump, or overflow behavior (`intercepts`, `spechit`,
  `visplanes`). When linuxdoom simply crashes or corrupts memory, we take
  Chocolate Doom's emulated behavior.

Where the two disagree on defined behavior, linuxdoom-1.10 wins.

## Goals

1. **Faithful to linuxdoom-1.10.** Same fixed-point math (including its
   `double`-based `FixedDiv`), same tables, same RNG, same tic order. The bar is
   that **the port's per-tic trace matches the headless linuxdoom-1.10 oracle
   bit-for-bit** while it plays the shareware demos (DEMO1–3 in `doom1.wad`). A
   demo records input only, so any divergence in the simulation shows up
   immediately. That gives us a free, strict end-to-end test.
2. **Readable against the C.** Every C module maps to one Scheme library with the
   same name, and every C function maps to a Scheme procedure with a predictable
   name (below). You should be able to open `p_map.c` and `src/p/map.sls` side by
   side and follow along.
3. **Plays at 35 Hz at 320×200 first.** Native-compiled Scheme should do this
   comfortably. A higher-resolution mode and an uncapped, interpolated framerate
   are planned for later (see *Future: hi-res and uncapped*). We don't build
   them now, but we avoid choices that would block them.

Out of scope for now: netplay (IPX/serial), the DOS sound drivers, and the
sound server process. We keep the `ticcmd_t` architecture because demos need it.

## Implementation: Chez Scheme 10 + SDL2

| Option | Why / why not |
|---|---|
| **Chez Scheme (chosen)** | Best native code of any Scheme; unboxed fixnum and flonum ops (`fx+`, `fxsra`, …); bytevectors for framebuffers and lumps; `foreign-procedure` makes SDL2 bindings a few dozen lines with no C glue. R6RS libraries give us real modules. |
| Guile 3 | JIT is decent, but the hot loops (column and span drawers) would be several times slower. |
| CHICKEN 5 | Compiles via C, which is fast enough, but it's a heavier toolchain and the FFI is less direct. |
| Racket CS | Built on Chez, but its runtime adds overhead and a larger surface area. |

The dev environment is a Nix flake (`flake.nix`) that provides `chez` and `SDL2`.

## Layout

```
reference/doom/          git submodule: id-software/DOOM — linuxdoom-1.10 is the target
reference/chocolate-doom/ git submodule: Chocolate Doom 3.1.1 — second reference
src/
  doom/                  one R6RS library per C module
    fixed.sls            m_fixed.c   (FixedMul, FixedDiv)
    tables.sls           tables.c    (generated)
    info.sls             info.c      (generated: states, mobjinfo, sprnames)
    random.sls           m_random.c
    wad.sls              w_wad.c
    ...
  i/                     platform layer (replaces i_video/i_sound/i_system/i_net)
    sdl.sls              raw SDL2 FFI bindings
    video.sls, sound.sls, system.sls, input.sls
tools/
  c2scm.py               converts C data tables (tables.c, info.c, sounds.c) to .sls
  oracle/                headless C reference build that emits tic traces
tests/
wads/                    gitignored; `make wad` fetches doom1.wad (shareware)
```

### Naming convention

`P_XYDist` → `p-xy-dist`, `R_RenderPlayerView` → `r-render-player-view`,
`mobj_t` → record `mobj`, field `momx` → accessor `mobj-momx` and mutator
`mobj-momx-set!`. Constants keep their C spelling in uppercase, e.g. `FRACUNIT`
and `MF_SOLID`. The C prefix tells you the module.

## Core representation choices

**Integers and wraparound.** Doom relies on 32-bit overflow in two places:
`angle_t` is an unsigned 32-bit value that wraps constantly, and some
`fixed_t` (signed 16.16) arithmetic overflows. Chez fixnums are 61-bit, so:
- Angles: every angle add or subtract goes through `(angle+ a b)`, which applies
  `(fxand … #xFFFFFFFF)`. A small `angle` library owns this.
- Fixed point: `fixed-mul` is `(fxsra (* a b) 16)` wrapped to int32.
  The product of two int32 values can exceed fixnum range, so we use generic
  `*` there, which handles the rare bignum case correctly. We profile it before
  doing anything cleverer.
- `wrap32` gets applied wherever the C silently overflows. We find those places
  by diffing demo traces, not by guessing.

**FixedDiv.** linuxdoom-1.10's `FixedDiv2` computes
`(fixed_t)((double)a / (double)b * FRACUNIT)` (the integer version is `#if 0`'d
out), and we match it exactly:
`(flonum->fixnum (fl* (fl/ (fixnum->flonum a) (fixnum->flonum b)) 65536.0))`,
truncating toward zero like a C cast. It differs from DOS v1.9's integer
division in rare low-bit cases. That is fine, because our target is linuxdoom,
not v1.9.

**Structs → mutable records.** `mobj_t`, `player_t`, `sector_t`, `line_t`,
`seg_t`, and the others become `define-record-type` with mutable fields. Hot
render-side data (vertex coordinates, column offsets) may be kept in parallel
fxvectors instead, if profiling says so.

**Thinkers.** The C code keeps an intrusive doubly-linked list of
`{prev, next, function}`, where removal sets the function pointer to -1. We keep
a doubly-linked list of thinker records with a `func` field that holds a
procedure or `'removed`. Iteration order matters for sync, so we preserve the C
semantics exactly, including deferred removal.

**State table.** `info.c` holds about 1,000 `state_t` entries whose action
fields are function pointers (`A_Chase`, `A_FirePistol`, …). The generator emits
symbols for these, and `info.sls` resolves them to procedures at load time
through a registry that each `p-*` library populates. That avoids a
circular-import knot.

**Memory / zone.** `z_zone.c` is dropped and the GC replaces it. Purge tags
(`PU_LEVEL` and the like) go away, because level data is simply dropped on
reload. `W_CacheLumpNum` becomes a memoizing lump→bytevector cache.

**Framebuffers.** `screens[0..4]` are `SCREENWIDTH*SCREENHEIGHT`-byte bytevectors of palette
indices, exactly like the C code. The renderer writes palette indices. The
platform layer converts to ARGB once per frame through a 256-entry palette
lookup and uploads to an SDL streaming texture. Gamma and palette flashes
(damage, pickup) are just a palette swap, as in the original.

**Endianness/parsing.** WAD lumps are read with `bytevector-s16-ref` and related
procedures using `(endianness little)`, so no `m_swap` is needed.

## Platform layer (SDL2 via Chez FFI)

- **Video:** a window with a 320×200 streaming texture, scaled with aspect
  correction (4:3).
- **Input:** SDL events become `event_t` (`ev_keydown`/`ev_keyup`/`ev_mouse`)
  and get fed to `D_PostEvent`. The key mapping comes from `i_video.c`'s
  `xlatekey`.
- **Timing:** `I_GetTime` returns tics at 35 Hz from `SDL_GetTicks`.
- **Sound:** SFX mixing is done in Scheme (8-bit mono DMX lumps resampled into a
  stereo mix buffer). The mix is pushed with `SDL_QueueAudio` from the main
  thread each tic, which avoids audio-thread callbacks into Scheme.
- **Music:** MUS→MIDI plus a synth is a stretch goal (options: OPL emulation, or
  hand MIDI to SDL_mixer or FluidSynth).

## Verification: the C oracle

The design's centerpiece is a **headless reference build of the C code** in
`tools/oracle/`. It is linuxdoom-1.10 with the X11, sound, and network layers
stubbed out. Its simulation is left **unpatched**, so the oracle *is* the
target. The harness touches the original in only three ways:
- **Demo version check** (`tools/oracle/oracle.patch`). `G_DoPlayDemo` rejects
  demo version ≠ 110, but the shareware demos are version 109. The oracle
  accepts 109, and so does our port.
- **Trace hook** (same patch). `D_DoomLoop` calls `oracle_tic()` after
  `D_Display`.
- **Built 32-bit** (`gcc -m32` via `$ORACLE_CC` from the flake). The 1993 code
  assumes 32-bit pointers, and this way nothing in the original changes.

`i_video.c` and `i_sound.c` are replaced by null layers. Rendering still happens
into `screens[0]`, and the palette is captured.

Whether the oracle itself stays in sync with the v1.9 recordings doesn't matter
to the gate. We only need to match the oracle. In practice the simulations
should be nearly identical.

Under `-timedemo`, which advances exactly one tic per frame, it writes one
trace line per tic. The exact format and hash definitions are documented in
`tools/oracle/oracle.c`:

```
gametic gamestate leveltime prndindex rndindex
px py pz pangle pmomx pmomy health armor
nmobjs mobjhash sectorhash framehash palettehash
```

It can also dump `screens[0]` as PPM every N tics. Run
`make oracle-traces` to get the traces. DEMO1, DEMO2, and DEMO3 run 5026, 3836,
and 2134 tics, in about 2 s each. The Scheme port emits the identical trace. `make sync-test` runs both on DEMO1–3 and reports the **first
divergent tic** and which field diverged. That turns "the demo desynced
somewhere" into "on tic 412, `rndindex` differs, so an extra `P_Random` call
happened". Renderer correctness gets the same treatment by comparing
framebuffers byte-for-byte on demo tics.

### Garbage reads

Vanilla occasionally renders from memory it doesn't own. The first case we
found is DEMO1, tic 606: a 1-pixel sprite column at the bottom edge of the view
(the shotgun psprite). There, `dc_texturemid + (dc_yl-centery)*dc_iscale` goes
negative, `(frac>>16)&127` wraps to texel 127, and the read lands past the end
of the sprite post. It lands in the zone allocator's block header, which holds
a byte of a heap pointer. As a result:
- **The oracle runs with ASLR off** (`setarch -R`, in `tools/oracle/run.sh`).
  With ASLR on, that pixel changes from run to run. With it off, traces are
  byte-identical across runs.
- **The port can't reproduce these pixels**, because they are bytes of C heap
  pointers. Simulation fields are a hard gate. `framehash` is a hard gate
  except on an explicit allowlist of (demo, tic) entries. Each entry needs a
  pixel diff showing the mismatch is confined to identified garbage reads.
  Reads past the end of a lump return 0 in the port. Chocolate Doom has the
  same behavior here, so there is no better-defined value to copy.

Smaller layers get unit tests against values extracted from C: fixed math,
`R_PointToAngle`, and `P_AproxDistance`.

## Milestones

Each milestone ends with something visible or testable.

0. **Bootstrap:** flake, SDL window, WAD fetch, oracle builds and traces DEMO1.
1. **WAD + video:** `w_wad`, `v_video`, and the palette. Draws TITLEPIC and
   patches. Also `m_fixed`, `m_random`, and the generated `tables`.
2. **Level load + automap:** `p_setup` and `am_map`. You can see E1M1 as lines.
3. **Software renderer:** `r_bsp`, `r_segs`, `r_plane`, `r_draw`, `r_sky`,
   `r_things`, and `r_data` (textures, flats, colormaps). We get a free-look
   static camera, and framebuffers diff against the oracle.
4. **Playsim core:** `info` (generated), `p_mobj`, `p_tick`, `p_user`,
   `p_maputl`, `p_map`, and `p_sight`. You can walk around and collide.
5. **World and monsters:** `p_spec`, doors, floors, ceilings, plats, lights,
   switches, teleporters, `p_enemy`, `p_pspr`, `p_inter`. The game is
   playable.
6. **Game loop + demos:** `g_game`, `d_main`, ticcmds. **DEMO1–3 stay in sync**
   (the main correctness gate).
7. **Front-end:** `st_stuff`, `hu_stuff`, `m_menu`, `wi_stuff`, `f_finale`,
   `f_wipe`.
8. **Sound:** `s_sound`, plus the SFX mixer, plus optionally music.
9. **Saves, config, polish:** `p_saveg`, `m_misc` (default.cfg), cheats,
   performance work.

Roughly 45k lines of C, about 10k of which is data (`info.c` and `tables.c`) that
gets generated rather than written.

## Future: hi-res and uncapped

Neither is built now, but these rules keep both cheap to add later.

- **Resolution is a parameter, not a literal.** `SCREENWIDTH`, `SCREENHEIGHT`,
  and everything derived from them (`viewwidth`, `centerx`, `yslope`,
  `distscale`, the screen-size-dependent tables in `r_main`) are computed in one
  place at startup. Nothing hardcodes 320, 200, 64000, or 168. Until hi-res
  lands, the only legal value is 320×200, because the oracle's framebuffer diffs
  assume it.
- **2D assets keep 320×200 coordinates.** `v_video`'s patch drawing takes
  virtual 320×200 coordinates and will scale to the real buffer later, so the
  menu, HUD, and intermission code never learns the real size.
- **Separate the simulation from rendering.** The playsim never reads anything
  render-side, and the renderer only *reads* playsim state. For uncapped
  framerate we will snapshot each mobj's, sector's, and the player's previous
  position, angle, and height each tic, then render at `prev + frac·(cur-prev)`,
  with `frac` from `I_GetTime` sub-tics. Keeping the renderer's inputs on a
  short, explicit list of fields (gathered in `r_main`'s setup) makes that
  change local.
- **Fidelity mode stays the default.** Hi-res and interpolation are display
  options. They never touch tic logic, so the sync test keeps working with them
  turned on.
