#' Connect to a store with full-text search enabled
#'
#' Opens the DuckDB store read-only and best-effort loads DuckDB's `fts`
#' extension on the connection. ragnar's hybrid retrieval relies on a
#' `match_bm25` macro that lives in `fts`, and DuckDB loads extensions
#' *per connection* -- so a fresh process can hit
#' `Catalog Error: Scalar Function with name match_bm25 does not exist` even
#' when the BM25 index is intact on disk. Loading `fts` here keeps hybrid
#' (vector + keyword) search working; keyword search matters for exact-token
#' queries an agent makes, like an error code or a function name. `LOAD` is
#' offline once the extension is installed, and a load failure is non-fatal
#' (logged, not raised) because the query path ([search_docs()]) still guards
#' retrieval and degrades to vector-only search.
#'
#' @param path Path to the DuckDB store created by [build_store()].
#'
#' @return A connected, read-only ragnar store object.
#' @seealso [build_store()], [search_docs()]
#' @export
#' @examples
#' \dontrun{
#' store <- connect_store("handbook.duckdb")
#' }
connect_store <- function(path) {
  store <- ragnar::ragnar_store_connect(path, read_only = TRUE)
  tryCatch(
    DBI::dbExecute(S7::prop(store, "con"), "LOAD fts;"),
    error = function(e) {
      message("ragdoc: could not load DuckDB 'fts' extension on this ",
              "connection -- keyword search may be unavailable (",
              conditionMessage(e), ")")
    }
  )
  store
}

#' Retrieve with graceful degradation
#'
#' Runs ragnar's hybrid retrieval (vector similarity + BM25 keyword search). If
#' the BM25 half fails -- typically because the `fts` extension is not loaded on
#' this connection and `match_bm25` is missing -- it loads `fts` and retries
#' hybrid search once, and only then degrades to vector-only retrieval,
#' emitting a warning so the quality drop is never silent.
#'
#' @param store A connected ragnar store (see [connect_store()]).
#' @param query Natural-language query string.
#' @param top_k Number of chunks to retrieve. Defaults to 8.
#'
#' @return A data frame of retrieved chunks (ragnar's retrieval result).
#' @keywords internal
#' @noRd
retrieve_resilient <- function(store, query, top_k = 8) {
  degrade <- function() {
    warning("BM25 retrieval unavailable; using vector-only search", call. = FALSE)
    ragnar::ragnar_retrieve_vss(store, query, top_k = top_k)
  }
  tryCatch(
    ragnar::ragnar_retrieve(store, query, top_k = top_k),
    error = function(e) {
      loaded <- tryCatch({
        DBI::dbExecute(S7::prop(store, "con"), "LOAD fts;")
        TRUE
      }, error = function(e2) FALSE)
      if (!loaded) return(degrade())
      tryCatch(
        ragnar::ragnar_retrieve(store, query, top_k = top_k),
        error = function(e2) degrade()
      )
    }
  )
}

# Flatten a possibly-list column to an atomic character vector. ragnar's hybrid
# retrieval returns `source`/`url` as list-columns -- a chunk matched by both the
# vector and BM25 halves carries its (identical) value duplicated into one cell,
# so cells are length 1+. Vector-only retrieval returns plain atomic columns.
# Collapse each cell to its distinct value(s) so downstream filtering/formatting
# is uniform across both retrieval shapes.
flatten_col <- function(col) {
  if (!is.list(col)) return(col)
  vapply(col, function(x) {
    u <- unique(as.character(x))
    if (length(u)) paste(u, collapse = ", ") else NA_character_
  }, character(1))
}

#' Filter and cap retrieved chunks
#'
#' Normalizes the `source`/`url` columns to atomic (hybrid retrieval returns them
#' as list-columns), optionally filters to a single `source`, then caps the
#' result at `n` rows.
#'
#' @param res A retrieval result data frame.
#' @param n Maximum number of chunks to keep. Defaults to 8.
#' @param source Optional source name to filter to.
#'
#' @return The filtered, capped data frame with atomic `source`/`url`.
#' @keywords internal
#' @noRd
select_chunks <- function(res, n = 8, source = NULL) {
  if (!is.null(res$source)) res$source <- flatten_col(res$source)
  if (!is.null(res$url)) res$url <- flatten_col(res$url)
  if (!is.null(source)) {
    # `==` against an NA source yields NA, and an NA logical row-index injects a
    # spurious all-NA row -- guard so a chunk with a missing source is dropped,
    # not turned into a phantom match.
    keep <- !is.na(res$source) & res$source == source
    res <- res[keep, , drop = FALSE]
  }
  utils::head(res, n)
}

#' Format retrieved chunks into a citable text payload
#'
#' Renders chunks into a single string, each passage prefixed with a
#' source-and-URL citation (`[source <dot> url]`) and separated by a rule. Long
#' passages are truncated to `max_chars`.
#'
#' @param chunks A data frame of chunks with `text`, `source`, and `url` columns.
#' @param max_chars Per-chunk character cap before truncation. Defaults to 1500.
#'
#' @return A single character scalar, or `"no matches"` when `chunks` is empty.
#' @keywords internal
#' @noRd
format_chunks <- function(chunks, max_chars = 1500) {
  if (nrow(chunks) == 0) return("no matches")
  parts <- vapply(seq_len(nrow(chunks)), function(i) {
    # Coerce defensively: like source/url, `text` can arrive as a list-column
    # cell holding a length-1+ vector, which would make `nchar()` error.
    txt <- paste(as.character(chunks$text[[i]]), collapse = " ")
    if (nchar(txt) > max_chars) txt <- paste0(substr(txt, 1, max_chars), " \u2026")
    sprintf("[%s \u00b7 %s]\n%s", chunks$source[[i]], chunks$url[[i]], txt)
  }, character(1))
  paste(parts, collapse = "\n\n---\n\n")
}

#' Search the documentation store
#'
#' The core retrieval entrypoint, and what [serve_mcp()] exposes to a coding
#' agent. Retrieves the most relevant passages for `query` from the store,
#' optionally restricted to one `source`, and formats them with source-and-URL
#' citations.
#'
#' When a `source` filter is given, retrieval over-fetches (`n * 4`) before
#' filtering so the cap still yields up to `n` in-source chunks. Retrieval is
#' network- and API-backed; failures are caught and returned as a short message
#' rather than raised, so a server built on this never crashes.
#'
#' @param store A connected ragnar store (see [connect_store()]).
#' @param query Natural-language search query.
#' @param n Number of chunks to return. Defaults to 8.
#' @param source Optional source name to restrict to (one of the names you gave
#'   to [web()]).
#'
#' @return A single formatted string of cited passages, `"no matches"`, or a
#'   `"retrieval failed: ..."` message.
#' @seealso [connect_store()], [serve_mcp()]
#' @export
#' @examples
#' \dontrun{
#' store <- connect_store("handbook.duckdb")
#' cat(search_docs(store, "how do I rotate the signing keys", n = 5))
#' }
search_docs <- function(store, query, n = 8, source = NULL) {
  # When a source filter is set, over-fetch (n * 4) so the post-filter cap can
  # still reach n in-source chunks. A fixed heuristic: a store dominated by
  # other sources may still under-fill n -- acceptable, since search_docs is a
  # best-effort retrieval tool, not a guaranteed-count API.
  res <- tryCatch(
    retrieve_resilient(store, query, top_k = if (is.null(source)) n else n * 4L),
    error = function(e) structure(list(), class = "ragnar_retrieve_error",
                                  message = conditionMessage(e))
  )
  if (inherits(res, "ragnar_retrieve_error")) {
    # Log the raw error server-side; return a generic message so the MCP caller
    # never sees internal filesystem paths or SQL from ragnar/DuckDB.
    message("ragdoc retrieval error: ", attr(res, "message"))
    return("retrieval failed: internal error (see server logs)")
  }
  format_chunks(select_chunks(res, n = n, source = source))
}
