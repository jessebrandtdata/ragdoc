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

test_that("ingest_source reports link-discovery failure and returns FALSE", {
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

test_that("ingest_source counts empty pages without marking them skipped", {
  local_mocked_bindings(
    ragnar_find_links = function(...) c("https://a.example/full.html", "https://a.example/empty.html"),
    ragnar_store_insert = function(store, chunks) invisible(NULL),
    .package = "ragnar"
  )
  local_mocked_bindings(
    read_page_chunks = function(page) {
      if (grepl("empty", page)) {
        data.frame(text = character(0), stringsAsFactors = FALSE)
      } else {
        data.frame(text = "chunk", stringsAsFactors = FALSE)
      }
    }
  )
  s <- web("a", "https://a.example/")
  expect_message(res <- ingest_source(store = NULL, source = s), "empty")
  expect_true(res)  # an empty page is not a skipped (failed) page
})

test_that("ingest_source reports 0 links and returns FALSE when nothing matches", {
  local_mocked_bindings(
    ragnar_find_links = function(...) character(),
    .package = "ragnar"
  )
  s <- web("a", "https://a.example/", pattern = "nomatch")
  expect_message(res <- ingest_source(store = NULL, source = s), "0 links")
  expect_false(res)
})

test_that("ingest_source ingests every page and returns TRUE", {
  inserted <- character()
  local_mocked_bindings(
    ragnar_find_links = function(...) c("https://a.example/1.html", "https://a.example/2.html"),
    ragnar_store_insert = function(store, chunks) {
      inserted <<- c(inserted, chunks$url)
      invisible(NULL)
    },
    .package = "ragnar"
  )
  local_mocked_bindings(
    read_page_chunks = function(page) data.frame(text = "chunk", stringsAsFactors = FALSE)
  )
  s <- web("a", "https://a.example/")
  res <- suppressMessages(ingest_source(store = NULL, source = s))
  expect_true(res)
  expect_length(inserted, 2)
})

test_that("ingest_source isolates a broken page and returns FALSE", {
  local_mocked_bindings(
    ragnar_find_links = function(...) c("https://a.example/ok.html", "https://a.example/bad.html"),
    ragnar_store_insert = function(store, chunks) invisible(NULL),
    .package = "ragnar"
  )
  local_mocked_bindings(
    read_page_chunks = function(page) {
      if (grepl("bad", page)) stop("boom")
      data.frame(text = "chunk", stringsAsFactors = FALSE)
    }
  )
  s <- web("a", "https://a.example/")
  expect_message(res <- ingest_source(store = NULL, source = s), "skip")
  expect_false(res)
})

test_that("ingest_local reports a missing directory and returns FALSE", {
  s <- local_dir("a", file.path(tempdir(), "does-not-exist-xyz"))
  expect_message(res <- ingest_source(store = NULL, source = s), "directory")
  expect_false(res)
})

test_that("ingest_local reports an empty match set and returns FALSE", {
  dir <- withr::local_tempdir()
  writeLines("not markdown", file.path(dir, "readme.txt"))
  s <- local_dir("a", dir)  # default pattern only matches .md
  expect_message(res <- ingest_source(store = NULL, source = s), "no files")
  expect_false(res)
})

test_that("ingest_local ingests every matching file, tagging source and url", {
  dir <- withr::local_tempdir()
  writeLines("# one", file.path(dir, "one.md"))
  writeLines("# two", file.path(dir, "two.md"))
  writeLines("ignored", file.path(dir, "skip.txt"))
  inserted <- list()
  local_mocked_bindings(
    ragnar_store_insert = function(store, chunks) {
      inserted[[length(inserted) + 1L]] <<- chunks
      invisible(NULL)
    },
    .package = "ragnar"
  )
  local_mocked_bindings(
    read_file_chunks = function(file) data.frame(text = "chunk", stringsAsFactors = FALSE)
  )
  s <- local_dir("notes", dir)
  res <- suppressMessages(ingest_source(store = NULL, source = s))
  expect_true(res)
  expect_length(inserted, 2)  # the .txt file is not matched
  expect_true(all(vapply(inserted, function(c) c$source, character(1)) == "notes"))
  urls <- vapply(inserted, function(c) c$url, character(1))
  expect_setequal(basename(urls), c("one.md", "two.md"))
})

test_that("ingest_local counts empty files without marking them skipped", {
  dir <- withr::local_tempdir()
  writeLines("# full", file.path(dir, "full.md"))
  writeLines("# empty", file.path(dir, "empty.md"))
  local_mocked_bindings(
    ragnar_store_insert = function(store, chunks) invisible(NULL),
    .package = "ragnar"
  )
  local_mocked_bindings(
    read_file_chunks = function(file) {
      if (grepl("empty", file)) {
        data.frame(text = character(0), stringsAsFactors = FALSE)
      } else {
        data.frame(text = "chunk", stringsAsFactors = FALSE)
      }
    }
  )
  s <- local_dir("notes", dir)
  expect_message(res <- ingest_source(store = NULL, source = s), "empty")
  expect_true(res)  # an empty file is not a skipped (failed) file
})

test_that("ingest_local isolates a broken file and returns FALSE", {
  dir <- withr::local_tempdir()
  writeLines("# ok", file.path(dir, "ok.md"))
  writeLines("# bad", file.path(dir, "bad.md"))
  local_mocked_bindings(
    ragnar_store_insert = function(store, chunks) invisible(NULL),
    .package = "ragnar"
  )
  local_mocked_bindings(
    read_file_chunks = function(file) {
      if (grepl("bad", file)) stop("boom")
      data.frame(text = "chunk", stringsAsFactors = FALSE)
    }
  )
  s <- local_dir("notes", dir)
  expect_message(res <- ingest_source(store = NULL, source = s), "skip")
  expect_false(res)
})

test_that("ingest_local honors recursive = FALSE", {
  dir <- withr::local_tempdir()
  writeLines("# top", file.path(dir, "top.md"))
  sub <- file.path(dir, "nested")
  dir.create(sub)
  writeLines("# deep", file.path(sub, "deep.md"))
  seen <- character()
  local_mocked_bindings(
    ragnar_store_insert = function(store, chunks) {
      seen <<- c(seen, basename(chunks$url))
      invisible(NULL)
    },
    .package = "ragnar"
  )
  local_mocked_bindings(
    read_file_chunks = function(file) data.frame(text = "chunk", stringsAsFactors = FALSE)
  )
  s <- local_dir("notes", dir, recursive = FALSE)
  suppressMessages(ingest_source(store = NULL, source = s))
  expect_identical(seen, "top.md")  # the nested file is not reached
})

test_that("ingest_source errors on an unknown source type", {
  bogus <- structure(list(name = "x", type = "carrier-pigeon"), class = "ragdoc_source")
  expect_error(ingest_source(NULL, bogus), "unknown source type")
})

test_that("build_store isolates a failing source and still builds the index", {
  built <- FALSE
  local_mocked_bindings(
    ragnar_store_create = function(...) "fake-store",
    ragnar_find_links = function(root_url) if (grepl("bad", root_url)) stop("unreachable") else "https://ok.example/1.html",
    ragnar_store_insert = function(store, chunks) invisible(NULL),
    ragnar_store_build_index = function(store) built <<- TRUE,
    .package = "ragnar"
  )
  local_mocked_bindings(
    read_page_chunks = function(page) data.frame(text = "chunk", stringsAsFactors = FALSE)
  )
  spec <- sources(web("ok", "https://ok.example/"), web("bad", "https://bad.example/"))
  res <- suppressMessages(build_store(spec, tempfile(), embed = function(x) x))
  expect_false(res)        # the bad source failed
  expect_true(built)       # but the index was still built
})
