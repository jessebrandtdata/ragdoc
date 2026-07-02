# Decision log

Design- and model-affecting choices for this project, from **2026-06-10** forward.
Reversible choices are EXECUTE-and-logged by agents; substantive ones are routed
through `dq` → `/decisions` and recorded here on resolution. This file is the
permanent, inspectable record of *why this project is shaped the way it is*.

**Provenance note:** anything in this repo predating this log is **unattributed and
not settled** — it may be Jesse's choice or an agent's, reviewed or not. `north_star.md`
(if present) is Jesse's, as of its date. Agents: do not cite pre-log code or structure
as "already decided" — promote load-bearing pre-log choices through `dq` (substantive)
or `dlog` (reversible) before building on them.
See `~/workspace/docs/decision-log.md` for the convention.

---

## 2026-06-10 — local_dir() constructor for local Markdown directory sources
- **Choice:** Add local_dir(name, path, pattern='\\.md$', recursive=TRUE) as a peer of web(); type='local'. Provisional name (local() shadows base R), flagged in PR for Jesse to settle.
- **Why:** README named local-directory indexing as the next roadmap item; mirrors web() so sources() composes both.
- **Reversible:** yes · **Decided by:** agent

## 2026-06-10 — Local sources record the file path in the url column
- **Choice:** For local files, set chunks$url to the file path so citations render [source · /path/file.md], reusing the existing url column and store schema (no schema change).
- **Why:** Keeps the store schema identical to web sources; the path is the natural citation locator a user recognizes.
- **Reversible:** yes · **Decided by:** agent

## 2026-06-10 — Shared ingest_items() loop across source types
- **Choice:** Extract the resilient per-item insert/tally loop into ingest_items(); ingest_source() dispatches on source$type to ingest_web()/ingest_local(), which only do discovery.
- **Why:** Avoids duplicating web's per-item failure-isolation logic into local; keeps the two source types genuinely isolated, sharing only a neutral primitive. Web messages preserved verbatim.
- **Reversible:** yes · **Decided by:** agent

## 2026-07-01 — local_dir() keeps its name
- **Choice:** Public constructor for local-directory sources stays local_dir() (not files()/dir()/local_files())
- **Why:** Jesse's call on public API naming; local() shadows base R, local_dir() is the unambiguous peer of web()
- **Reversible:** no · **Decided by:** jesse

## 2026-07-01 — Route build+refresh through ragnar_store_ingest
- **Choice:** build_store() and new refresh_store() both ingest via ragnar::ragnar_store_ingest() + per-source prepare closures, replacing the hand-rolled ragnar_store_insert loop
- **Why:** ingest gives content-aware dedup (only new/changed pages re-embed, verified), per-item error isolation, and origin tracking for free; insert hard-errors on duplicate origin and cannot do incremental refresh. Aligns with north_star 'thin wrapper of ragnar' and matches r-knowledge-rag's proven mechanism
- **Reversible:** yes · **Decided by:** agent

## 2026-07-01 — refresh_store() as a separate verb
- **Choice:** Incremental refresh is a new exported refresh_store(sources, path); build_store keeps its guard (errors if store exists unless overwrite=TRUE)
- **Why:** Two clear verbs (build once / refresh often) read better than overloading overwrite=FALSE; preserves the anti-clobber guard
- **Reversible:** yes · **Decided by:** agent

## 2026-07-01 — Multi-format local_dir ingestion
- **Choice:** local_dir() default pattern becomes NULL (all files); ingest dispatches by extension: text/md verbatim, source code wrapped in a language-fenced block, everything else via read_as_markdown()/MarkItDown; local files over 25MB are skipped
- **Why:** Ports coursework's proven multi-format reader so a personal notes/slides/pdf corpus indexes well; size cap prevents MarkItDown OOM
- **Reversible:** yes · **Decided by:** agent

## 2026-07-01 — load_sources() YAML reader; deps
- **Choice:** Add exported load_sources(path) turning sources.yml into a sources() spec (dispatch web vs local by root_url/path). mirai and yaml both go in Suggests with a requireNamespace() guard (mirai checked up front in build/refresh via check_ingest_deps(); yaml in load_sources) — mirai is used only transitively via ragnar_store_ingest, so Imports would trip an "imported but unused" R CMD check NOTE, and Suggests+guard mirrors ragdoc's existing ellmer/mcptools pattern
- **Why:** Lets instances stay declarative; keeps the hard-dependency surface minimal and consistent with the package's existing optional-dependency style
- **Reversible:** yes · **Decided by:** agent

## 2026-07-02 — build_store/refresh_store gain a memory_limit knob
- **Choice:** Add memory_limit= (size string like '12GB') to build_store() and refresh_store(); it runs SET memory_limit on the store connection before ingest so the whole build — including the HNSW vector-index step — can use more than the engine's auto-cap. Strictly validated (injection-safe). NULL default keeps the engine's default. **Named by function, not tech** — Jesse's refinement: the arg is `memory_limit`, not `duckdb_memory`, so the public API isn't pinned to DuckDB.
- **Why:** DuckDB auto-caps memory at ~80% of RAM; under a constrained cgroup that cap is too low and the index build OOMs on a large store (hit on the 37k-chunk coursework rebuild). Jesse decided ragdoc owns this rather than the instance build scripts, and that the arg be named for its function.
- **Reversible:** no · **Decided by:** jesse
