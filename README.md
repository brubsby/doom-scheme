# doom-scheme

DOOM ported to Chez Scheme, faithful to linuxdoom-1.10. See [DESIGN.md](DESIGN.md).

Reference sources are git submodules:
- `reference/doom/`: id Software's release; `linuxdoom-1.10/` is the target.
- `reference/chocolate-doom/`: Chocolate Doom 3.1.1, the second reference.

```sh
git clone --recursive https://github.com/brubsby/doom-scheme.git
cd doom-scheme
nix develop
make wad            # fetch + verify the v1.9 shareware doom1.wad into wads/
make run            # DOOM: title loop, menus (ESC), the shareware episode
make run ARGS="-warp 1 3 -skill 4"   # any of linuxdoom's command line
make test           # Scheme vs. oracle: the renderer, the playsim, and every
                    # frame of demos, menus, intermission and finale
make oracle-traces  # build headless linuxdoom, trace DEMO1-3 into build/oracle/
```
