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

test_that("memory_limit must be a valid size string (and is injection-safe)", {
  expect_invisible(apply_memory_limit(NULL, NULL))               # NULL is a no-op
  expect_error(apply_memory_limit(NULL, "lots"), "size string")
  expect_error(apply_memory_limit(NULL, "12"), "size string")   # no unit
  expect_error(apply_memory_limit(NULL, 12), "size string")     # not a string
  expect_error(apply_memory_limit(NULL, "8GB'; DROP TABLE chunks; --"), "size string")
})

test_that("apply_memory_limit sets the store engine's memory limit on the connection", {
  skip_on_cran()
  fake_embed <- function(x) matrix(0, nrow = length(x), ncol = 4L)
  path <- withr::local_tempfile(fileext = ".duckdb")
  store <- ragnar::ragnar_store_create(
    path, embed = fake_embed,
    extra_cols = data.frame(source = character(), url = character()), overwrite = TRUE
  )
  con <- S7::prop(store, "con")
  before <- DBI::dbGetQuery(con, "SELECT current_setting('memory_limit') AS m")$m
  apply_memory_limit(store, "1GB")
  after <- DBI::dbGetQuery(con, "SELECT current_setting('memory_limit') AS m")$m
  expect_false(identical(before, after))  # the limit changed
  expect_match(after, "[0-9]")            # to a concrete size
  DBI::dbDisconnect(con, shutdown = TRUE)
})
