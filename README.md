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
make run            # title pages (any key: next page, ESC: quit)
make test           # Scheme vs. oracle: fixed math, tables, WAD + patch drawing
make oracle-traces  # build headless linuxdoom, trace DEMO1-3 into build/oracle/
```
