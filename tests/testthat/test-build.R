test_that("build_store errors without OPENAI_API_KEY when embed is NULL", {
  withr::local_envvar(OPENAI_API_KEY = "")
  expect_error(
    build_store(sources(web("a", "https://a.example/")), tempfile()),
    "OPENAI_API_KEY"
  )
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
