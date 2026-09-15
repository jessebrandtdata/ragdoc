# --- prepare functions (real ragnar, offline: markdown_chunk/MarkdownDocument) ---

test_that("local_prepare_fn reads text/markdown verbatim and tags source and url", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "note.md")
  writeLines(c("# Notes", "", "Cross validation explained here at some length."), f)
  ch <- local_prepare_fn("notes")(f)
  expect_identical(unique(ch$source), "notes")
  expect_identical(unique(ch$url), f)
  expect_no_match(paste(ch$text, collapse = "\n"), "```", fixed = TRUE)
})

test_that("local_prepare_fn wraps source code in a language-fenced block", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "script.R")
  writeLines(c("fit <- lm(y ~ x, data = d)", "summary(fit)"), f)
  ch <- local_prepare_fn("code")(f)
  expect_match(paste(ch$text, collapse = "\n"), "```r", fixed = TRUE)
  expect_identical(unique(ch$url), f)
})

test_that("local_prepare_fn maps known code extensions to their language fence", {
  dir <- withr::local_tempdir()
  py <- file.path(dir, "s.py")
  writeLines(c("import os", "print(os.getcwd())", "x = 1 + 2"), py)
  expect_match(paste(local_prepare_fn("c")(py)$text, collapse = "\n"),
               "```python", fixed = TRUE)
  sql <- file.path(dir, "q.sql")
  writeLines(c("select * from t", "where id = 1"), sql)
  expect_match(paste(local_prepare_fn("c")(sql)$text, collapse = "\n"),
               "```sql", fixed = TRUE)
})

test_that("local_prepare_fn uses a bare fence for code with no language mapping", {
  dir <- withr::local_tempdir()
  go <- file.path(dir, "m.go")
  writeLines(c("package main", "func main() { println(\"hi\") }"), go)
  txt <- paste(local_prepare_fn("c")(go)$text, collapse = "\n")
  expect_match(txt, "```", fixed = TRUE)        # still fenced as code
  expect_no_match(txt, "```go", fixed = TRUE)   # but with no language label
})

test_that("local_prepare_fn matches extensions case-insensitively", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "S.PY")
  writeLines(c("import sys", "print(sys.version)"), f)
  expect_match(paste(local_prepare_fn("c")(f)$text, collapse = "\n"),
               "```python", fixed = TRUE)
})

test_that("local_prepare_fn reads .txt verbatim, not as a fenced code block", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "readme.txt")
  writeLines(c("plain text notes about widgets", "second line of prose here"), f)
  txt <- paste(local_prepare_fn("notes")(f)$text, collapse = "\n")
  expect_no_match(txt, "```", fixed = TRUE)
})

# --- web discovery + dispatch (ragnar mocked, offline) ----------------------

test_that("ingest_web reports link-discovery failure and returns FALSE", {
  local_mocked_bindings(
    ragnar_find_links = function(...) stop("DNS boom"),
    .package = "ragnar"
  )
  expect_message(
    res <- ingest_source(store = NULL, source = web("a", "https://a.example/")),
    "link discovery failed"
  )
  expect_false(res)
})

test_that("ingest_web reports 0 links and returns FALSE when nothing matches", {
  local_mocked_bindings(
    ragnar_find_links = function(...) character(),
    .package = "ragnar"
  )
  s <- web("a", "https://a.example/", pattern = "nomatch")
  expect_message(res <- ingest_source(store = NULL, source = s), "0 links")
  expect_false(res)
})

test_that("ingest_web hands the discovered links to the ingester and returns TRUE", {
  seen <- NULL
  local_mocked_bindings(
    ragnar_find_links = function(...) c("https://a.example/1.html", "https://a.example/2.html"),
    ragnar_store_ingest = function(store, paths, ...) { seen <<- paths; invisible(store) },
    .package = "ragnar"
  )
  res <- suppressMessages(ingest_source(store = NULL, source = web("a", "https://a.example/")))
  expect_true(res)
  expect_length(seen, 2)
})

# --- local discovery + dispatch (ragnar mocked, offline) --------------------

test_that("ingest_local reports a missing directory and returns FALSE", {
  s <- local_dir("a", file.path(tempdir(), "does-not-exist-xyz"))
  expect_message(res <- ingest_source(store = NULL, source = s), "directory")
  expect_false(res)
})

test_that("ingest_local reports an empty match set and returns FALSE", {
  dir <- withr::local_tempdir()  # empty directory
  expect_message(res <- ingest_source(store = NULL, source = local_dir("a", dir)), "no files")
  expect_false(res)
})

test_that("ingest_local takes every file by default and honors a pattern", {
  dir <- withr::local_tempdir()
  writeLines("# one", file.path(dir, "one.md"))
  writeLines("x <- 1", file.path(dir, "two.R"))
  writeLines("some data", file.path(dir, "three.txt"))
  seen <- NULL
  local_mocked_bindings(
    ragnar_store_ingest = function(store, paths, ...) { seen <<- paths; invisible(store) },
    .package = "ragnar"
  )
  suppressMessages(ingest_source(NULL, local_dir("notes", dir)))              # default: all files
  expect_setequal(basename(seen), c("one.md", "two.R", "three.txt"))
  seen <- NULL
  suppressMessages(ingest_source(NULL, local_dir("notes", dir, pattern = "\\.md$")))
  expect_setequal(basename(seen), "one.md")
})

test_that("ingest_local skips oversize files", {
  dir <- withr::local_tempdir()
  writeLines("# small", file.path(dir, "small.md"))
  writeBin(raw(26L * 1024L * 1024L), file.path(dir, "big.pdf"))  # 26 MB > 25 MB cap
  seen <- NULL
  local_mocked_bindings(
    ragnar_store_ingest = function(store, paths, ...) { seen <<- paths; invisible(store) },
    .package = "ragnar"
  )
  expect_message(suppressWarnings(ingest_source(NULL, local_dir("notes", dir))), "over 25 MB")
  expect_setequal(basename(seen), "small.md")
})

test_that("ingest_local honors recursive = FALSE", {
  dir <- withr::local_tempdir()
  writeLines("# top", file.path(dir, "top.md"))
  sub <- file.path(dir, "nested")
  dir.create(sub)
  writeLines("# deep", file.path(sub, "deep.md"))
  seen <- NULL
  local_mocked_bindings(
    ragnar_store_ingest = function(store, paths, ...) { seen <<- paths; invisible(store) },
    .package = "ragnar"
  )
  suppressMessages(ingest_source(NULL, local_dir("notes", dir, recursive = FALSE)))
  expect_identical(basename(seen), "top.md")  # the nested file is not reached
})

test_that("ingest_source errors on an unknown source type", {
  bogus <- structure(list(name = "x", type = "carrier-pigeon"), class = "ragdoc_source")
  expect_error(ingest_source(NULL, bogus), "unknown source type")
})

# --- build_store / refresh_store guards -------------------------------------

test_that("build_store errors without OPENAI_API_KEY when embed is NULL", {
  withr::local_envvar(OPENAI_API_KEY = "")
  expect_error(
    build_store(sources(web("a", "https://a.example/")), tempfile()),
    "OPENAI_API_KEY"
  )
})

test_that("build_store refuses to overwrite an existing store by default", {
  tmp <- withr::local_tempfile(fileext = ".duckdb")
  file.create(tmp)
  expect_error(
    build_store(sources(web("a", "https://a.example/")), tmp),
    "already exists"
  )
})

test_that("build_store rejects a non-function embed", {
  tmp <- withr::local_tempfile(fileext = ".duckdb")  # not created -> does not exist
  expect_error(
    build_store(sources(web("a", "https://a.example/")), tmp, embed = "nope"),
    "must be a function"
  )
})

test_that("resolve_embed returns a function for NULL when OPENAI_API_KEY is set", {
  withr::local_envvar(OPENAI_API_KEY = "sk-test-not-real")
  expect_type(resolve_embed(NULL), "closure")
})

test_that("resolve_embed passes a supplied embedder through unchanged", {
  my <- function(x) x
  expect_identical(resolve_embed(my), my)
})

test_that("build_store isolates a failing source and still builds the index", {
  built <- FALSE
  local_mocked_bindings(
    ragnar_store_create = function(...) "fake-store",
    ragnar_find_links = function(root_url) if (grepl("bad", root_url)) stop("unreachable") else "https://ok.example/1.html",
    ragnar_store_ingest = function(store, paths, ...) invisible(store),
    ragnar_store_build_index = function(store) built <<- TRUE,
    .package = "ragnar"
  )
  spec <- sources(web("ok", "https://ok.example/"), web("bad", "https://bad.example/"))
  res <- suppressMessages(build_store(spec, tempfile(), embed = function(x) x))
  expect_false(res)        # the bad source failed
  expect_true(built)       # but the index was still built
})

test_that("refresh_store errors when no store exists yet", {
  expect_error(
    refresh_store(sources(web("a", "https://a.example/")), tempfile(fileext = ".duckdb")),
    "no store"
  )
})
