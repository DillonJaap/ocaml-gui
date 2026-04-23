# AGENTS.md

## Build & Run

```bash
dune build          # build everything
dune exec bin/main.exe  # run the app
dune test           # run tests
```

No README. No CI. No pre-commit hooks.

## Project Structure

- `lib/` — `gui` library: `Levenshtein` (fuzzy match), `Os_util` (fs + daemonize)
- `bin/main.ml` — executable; opens `Gui` and `Core` at file scope
- `test/test_gui.ml` — currently empty

## Dependencies

- `raylib` + `raygui` — rendering (C bindings)
- `core` + `core_unix` — Jane Street stdlib replacement
- `ppx_deriving.show` — used in `bin/` only
- `spawn` — process spawning

## Unix / Fork

`lib/` uses `Core_unix.fork` (returns polymorphic variants `` `In_the_child `` / `` `In_the_parent of Pid.t ``). Stdlib `Unix.fork` returns `int` — do not mix them. Convert `Core.Pid.t` → `int` with `Core.Pid.to_int` when calling stdlib `Unix.*` functions.

## Formatter

`ocamlformat` with `profile = janestreet`, 80-char margin. Config in `.ocamlformat`. Run:

```bash
ocamlformat --inplace <file>
```

## Key Quirks

- `lib/dune` lists `(libraries core core_unix unix)` — all three in scope. Qualify calls explicitly to avoid ambiguity (prefer `Core_unix.*` over `Unix.*`).
- `dune-project` has placeholder author/source fields — do not treat as real metadata.
- `gui.opam` is generated from `dune-project`; edit the latter, not the former.
