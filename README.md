# dirscribe

Dirscribe is a small OCaml CLI that scans a directory and uses a Gemini model
to summarize files and subdirectories. It respects `.gitignore`, skips binary
files, and prints progress as it goes.

## Quick start

1) Create a dot directory and API key file:

```sh
mkdir -p ~/.dirscribe
printf "GEMINI_API_KEY=YOUR_KEY_HERE\n" > ~/.dirscribe/.env
```

2) Build and run:

```sh
dune build
dune exec dirscribe -- <path> -depth 2 -dot-dirscribe-path ~/.dirscribe
```

Notes:
- `-depth` must be a positive integer.
- Traversal depth is currently fixed to `1` in `lib/os_utils.ml` (the CLI flag
  is validated but not yet wired through).
- The `-dot-dirscribe-path` should expand in your shell; avoid quoting `~`.

## What it does

- Collects files/directories from the root path.
- Ignores `.git` and entries from `.gitignore`.
- Treats files as text when they are mostly printable ASCII.
- Sends prompts to Gemini and attaches summaries to each file/dir.

## Repo layout

- `bin/main.ml` CLI entrypoint.
- `lib/os_utils.ml` filesystem traversal and content loading.
- `lib/llm_utils.ml` Gemini request/response handling.
- `lib/pretty_print.ml` tree-style printing helpers.

## Development

```sh
opam install . --deps-only
dune build
```
