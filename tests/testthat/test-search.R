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

test_that("select_chunks caps at n with no filter", {
  sel <- select_chunks(fixture(), n = 2, source = NULL)
  expect_equal(nrow(sel), 2)
  expect_identical(sel$text[1], "chunk A")
})

test_that("select_chunks filters by source, then caps", {
  sel <- select_chunks(fixture(), n = 5, source = "handbook")
  expect_equal(nrow(sel), 2)
  expect_true(all(sel$source == "handbook"))
})

test_that("select_chunks returns no rows when the source is absent", {
  sel <- select_chunks(fixture(), n = 5, source = "nope")
  expect_equal(nrow(sel), 0)
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

  # filtering by source still works after flattening
  filtered <- select_chunks(hybrid, n = 5, source = "handbook")
  expect_equal(nrow(filtered), 1)
  expect_identical(filtered$source, "handbook")
})

test_that("format_chunks cites [source . url] and separates entries", {
  out <- format_chunks(select_chunks(fixture(), n = 5, source = "handbook"))
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

test_that("search_docs formats retrieved chunks", {
  local_mocked_bindings(
    ragnar_retrieve = function(store, query, top_k = 8) fixture(),
    .package = "ragnar"
  )
  out <- search_docs(store = NULL, query = "anything", n = 3)
  expect_match(out, "\\[handbook")
  expect_match(out, "\\[api")
})

test_that("search_docs over-fetches then filters when source is set", {
  seen_top_k <- NULL
  local_mocked_bindings(
    ragnar_retrieve = function(store, query, top_k = 8) {
      seen_top_k <<- top_k
      fixture()
    },
    .package = "ragnar"
  )
  out <- search_docs(store = NULL, query = "q", n = 2, source = "api")
  expect_equal(seen_top_k, 8)          # n * 4 over-fetch
  expect_match(out, "\\[api")
  expect_no_match(out, "\\[handbook")
})

test_that("search_docs reports retrieval failure instead of erroring", {
  local_mocked_bindings(
    ragnar_retrieve = function(...) stop("boom"),
    ragnar_retrieve_vss = function(...) stop("boom"),
    .package = "ragnar"
  )
  # No DBI connection on a NULL store, so the fts reload also fails and the
  # vector-only fallback throws too. The degrade warning along the way is
  # incidental here; we care that the failure surfaces as a message.
  out <- suppressWarnings(search_docs(store = NULL, query = "q"))
  expect_match(out, "retrieval failed")
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
