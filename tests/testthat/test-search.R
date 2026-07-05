# A small fixture mimicking a ragnar retrieval result.
fixture <- function() {
  data.frame(
    text   = c("chunk A", "chunk B", "chunk C"),
    source = c("handbook", "api", "handbook"),
    url    = c("https://docs.example.com/a.html",
               "https://api.example.com/b.html",
               "https://docs.example.com/c.html"),
    stringsAsFactors = FALSE
  )
}

test_that("select_chunks caps at n", {
  sel <- select_chunks(fixture(), n = 2)
  expect_equal(nrow(sel), 2)
  expect_identical(sel$text[1], "chunk A")
})

test_that("flatten_col passes an atomic column through unchanged", {
  x <- c("handbook", "api", "handbook")
  expect_identical(flatten_col(x), x)
})

test_that("flatten_col joins distinct values and maps an empty cell to NA", {
  # A list-column cell can hold repeated identical values (collapsed to one),
  # genuinely distinct values (joined), or nothing at all (-> NA).
  col <- list(c("handbook", "handbook"), c("handbook", "api"), character(0), "solo")
  out <- flatten_col(col)
  expect_identical(out[1], "handbook")          # duplicates collapse to one
  expect_identical(out[2], "handbook, api")     # distinct values are joined
  expect_true(is.na(out[3]))                    # empty cell -> NA
  expect_identical(out[4], "solo")
})

test_that("select_chunks flattens hybrid list-columns to atomic", {
  # ragnar hybrid retrieval returns source/url as list-columns; a chunk matched
  # by both vector + BM25 carries its value duplicated into a length-2 cell.
  hybrid <- data.frame(text = c("chunk A", "chunk B"), stringsAsFactors = FALSE)
  hybrid$source <- list(c("handbook", "handbook"), "api")        # length 2 and 1
  hybrid$url    <- list(c("https://docs.example.com/a.html", "https://docs.example.com/a.html"),
                        "https://api.example.com/b.html")

  sel <- select_chunks(hybrid, n = 5)
  expect_type(sel$source, "character")
  expect_type(sel$url, "character")
  expect_identical(sel$source, c("handbook", "api"))

  # and it then formats without error (the bug that hybrid success exposed)
  out <- format_chunks(sel)
  expect_match(out, "\\[handbook")
  expect_match(out, "\\[api")
})

test_that("format_chunks cites [source . url] and separates entries", {
  out <- format_chunks(select_chunks(fixture(), n = 5))
  expect_match(out, "\\[handbook")
  expect_match(out, "https://docs.example.com/a.html", fixed = TRUE)
  expect_match(out, "---", fixed = TRUE)
})

test_that("format_chunks returns 'no matches' for an empty frame", {
  expect_identical(format_chunks(fixture()[0, ]), "no matches")
})

test_that("format_chunks truncates long passages", {
  long <- data.frame(text = strrep("x", 5000), source = "handbook",
                     url = "https://docs.example.com/x.html", stringsAsFactors = FALSE)
  expect_match(format_chunks(long, max_chars = 100), "\u2026")
})

test_that("format_chunks omits the separator for a single chunk", {
  one <- data.frame(text = "the only chunk", source = "handbook",
                    url = "https://docs.example.com/a.html", stringsAsFactors = FALSE)
  out <- format_chunks(one)
  expect_match(out, "\\[handbook")
  expect_no_match(out, "---", fixed = TRUE)  # the rule only separates entries
})

test_that("format_chunks leaves a passage exactly at the cap untruncated", {
  chunk <- data.frame(text = strrep("x", 100), source = "s", url = "u",
                      stringsAsFactors = FALSE)
  # nchar == max_chars is not > max_chars, so no ellipsis is appended.
  expect_no_match(format_chunks(chunk, max_chars = 100), "\u2026")
})

test_that("search_docs formats retrieved chunks", {
  local_mocked_bindings(
    ragnar_retrieve = function(store, query, top_k = 8) fixture(),
    .package = "ragnar"
  )
  out <- search_docs(store = NULL, query = "anything", n = 3)
  expect_match(out, "\\[handbook")
  expect_match(out, "\\[api")
})

test_that("search_docs pushes the source filter into retrieval without over-fetching", {
  seen <- new.env()
  local_mocked_bindings(
    ragnar_retrieve = function(store, query, top_k = 8, ...) {
      seen$top_k <- top_k
      seen$has_filter <- "filter" %in% ...names()  # do not force the filter promise
      fixture()
    },
    .package = "ragnar"
  )
  search_docs(store = NULL, query = "q", n = 2, source = "api")
  expect_equal(seen$top_k, 2)     # asks for exactly n -- no n*4 over-fetch
  expect_true(seen$has_filter)    # source restriction pushed down as a filter
})

test_that("search_docs reports a generic failure and does not leak the raw error", {
  local_mocked_bindings(
    ragnar_retrieve = function(...) stop("secret/path/boom"),
    ragnar_retrieve_vss = function(...) stop("secret/path/boom"),
    .package = "ragnar"
  )
  # No DBI connection on a NULL store, so the fts reload also fails and the
  # vector-only fallback throws too. The raw error is logged via message(); the
  # caller-facing return is generic so internal paths never reach the agent.
  expect_message(
    suppressWarnings(search_docs(store = NULL, query = "q")),
    "boom"
  )
  out <- suppressWarnings(suppressMessages(search_docs(store = NULL, query = "q")))
  expect_match(out, "retrieval failed")
  expect_no_match(out, "boom")
})

test_that("format_chunks tolerates a list-column text cell", {
  chunks <- data.frame(source = "handbook", url = "u", stringsAsFactors = FALSE)
  chunks$text <- list(c("part one", "part two"))
  out <- format_chunks(chunks)
  expect_match(out, "part one")
  expect_match(out, "part two")
})

test_that("retrieve_resilient returns hybrid results when available", {
  local_mocked_bindings(
    ragnar_retrieve = function(store, query, top_k = 8) fixture(),
    .package = "ragnar"
  )
  expect_no_warning(res <- retrieve_resilient(store = NULL, query = "q", top_k = 3))
  expect_equal(nrow(res), 3)
})

test_that("retrieve_resilient loads fts, retries hybrid, and recovers without degrading", {
  # The primary path the fix exists for: hybrid fails once (match_bm25 missing),
  # LOAD fts succeeds, the retried hybrid call succeeds -> full hybrid results,
  # no vector-only fallback, no warning.
  calls <- 0L
  local_mocked_bindings(
    ragnar_retrieve = function(store, query, top_k = 8) {
      calls <<- calls + 1L
      if (calls == 1L) stop("Catalog Error: Scalar Function with name match_bm25 does not exist")
      fixture()
    },
    ragnar_retrieve_vss = function(...) stop("vss must not be reached on the recovery path"),
    .package = "ragnar"
  )
  # Make the fts reload succeed on a fake store.
  local_mocked_bindings(prop = function(object, name) "fake-con", .package = "S7")
  local_mocked_bindings(dbExecute = function(conn, statement, ...) 0L, .package = "DBI")

  expect_no_warning(res <- retrieve_resilient(store = "fake-store", query = "q", top_k = 3))
  expect_equal(nrow(res), 3)
  expect_equal(calls, 2L)  # one failure, one successful retry
})

test_that("retrieve_resilient degrades to vector-only with a warning", {
  # Hybrid fails; the fts reload can't run on a NULL store, so it must fall
  # through to vector-only retrieval and warn.
  local_mocked_bindings(
    ragnar_retrieve = function(...) stop("match_bm25 does not exist"),
    ragnar_retrieve_vss = function(store, query, top_k = 8) fixture()[1, ],
    .package = "ragnar"
  )
  expect_warning(
    res <- retrieve_resilient(store = NULL, query = "q"),
    "vector-only"
  )
  expect_equal(nrow(res), 1)
})
