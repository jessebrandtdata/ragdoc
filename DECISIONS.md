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
