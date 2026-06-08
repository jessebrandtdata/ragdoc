# ragdoc

> Point it at your docs, get an agent-ready search tool.

`ragdoc` turns a set of documentation sources into a hybrid-search RAG store and
serves it to coding agents over the [Model Context Protocol][mcp]. Instead of
guessing, an agent can retrieve the *actual* passages from your docs — each one
returned with a `[source · url]` citation it can quote back.

It's a thin, opinionated layer over [`ragnar`][ragnar] (embeddings + DuckDB +
retrieval). `ragdoc` adds the parts you'd otherwise rewrite every time: a
crawl-and-build pipeline with per-page error isolation, a connection that
*actually* keeps keyword search working, retrieval that degrades gracefully
instead of erroring, citation formatting, and one-line MCP serving.

## Install

```r
# install.packages("pak")
pak::pak("jesseabrandt/ragdoc")
```

You'll need an `OPENAI_API_KEY` in your environment (used to embed documents at
build time). Put it in `~/.Renviron`.

## Quick start

Describe where your docs live, build the store once, then query it:

```r
library(ragdoc)

# 1. Point it at YOUR docs. Each source is a site to crawl, with an optional
#    regex restricting which links under it get ingested.
src <- sources(
  web("handbook", "https://docs.example.com/", pattern = "\\.html$")
)

# 2. Build an embedded, hybrid-searchable store. One-time; needs OPENAI_API_KEY.
build_store(src, "handbook.duckdb")

# 3. Query it. Passages come back with [source · url] citations, agent-ready.
store <- connect_store("handbook.duckdb")
cat(search_docs(store, "how do I rotate the signing keys", n = 5))
```

`search_docs()` returns a single formatted string — the passages, each prefixed
with its source and URL and separated by a rule. That's what you hand to an
agent. Restrict to one source with `search_docs(store, query, source = "handbook")`.

## Serving over MCP

Most of the value is exposing the store to a coding agent. A complete MCP server
is three lines — drop this in a script (e.g. `mcp_server.R`) and point your agent
at it:

```r
library(ragdoc)
serve_mcp("handbook.duckdb",
  name        = "search_handbook",
  description = "Search the Example Co engineering handbook.")
```

The agent now has a `search_handbook` tool. `serve_mcp()` accepts either a store
path (shown here) or a connected store from `connect_store()`. Given a path, the
server starts even if the store hasn't been built yet — the tool reports that
clearly rather than crashing the server.

## How it works (and why the extra layer)

`ragdoc` exists to absorb four sharp edges of doing this by hand:

- **Build resilience.** One broken page or one unreachable source doesn't abort
  the crawl — failures are logged and skipped, and you still get a usable store.
- **Keyword search that stays on.** Hybrid retrieval (vector + BM25) needs
  DuckDB's `fts` extension loaded *per connection*. `connect_store()` does this,
  so exact-token queries (an error code, a function name) keep working in a fresh
  process instead of failing with a missing-`match_bm25` error.
- **Graceful degradation.** If the keyword half can't run, retrieval retries and
  then falls back to vector-only search with a warning — never a hard failure.
- **Citations, formatted.** Results come back ready to quote, not as a raw frame
  you have to reshape.

Outputs are standard objects — `connect_store()` hands back a `ragnar` store you
can query directly if you want to drop below `ragdoc`. No walled garden.

## Status

`0.1.0` — web-crawl sources, OpenAI embeddings (override with `embed=`).
Indexing local files (a directory of Markdown) is planned for a later release.

## License

MIT © Jesse Brandt

[mcp]: https://modelcontextprotocol.io
[ragnar]: https://ragnar.tidyverse.org
