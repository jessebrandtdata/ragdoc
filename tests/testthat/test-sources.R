test_that("web() builds a source spec with an optional pattern", {
  s <- web("handbook", "https://docs.example.com/", pattern = "\\.html$")
  expect_s3_class(s, "ragdoc_source")
  expect_identical(s$name, "handbook")
  expect_identical(s$root_url, "https://docs.example.com/")
  expect_identical(s$crawl_pattern, "\\.html$")
  expect_identical(s$type, "web")
})

test_that("web() leaves crawl_pattern NULL when omitted", {
  s <- web("handbook", "https://docs.example.com/")
  expect_null(s$crawl_pattern)
})

test_that("web() rejects empty or non-string name and root_url", {
  expect_error(web("", "https://x.example/"), "name")
  expect_error(web("ok", ""), "root_url")
  expect_error(web(1, "https://x.example/"), "name")
  expect_error(web("ok", "https://x.example/", pattern = 1), "pattern")
})

test_that("web() requires an http(s) URL scheme", {
  expect_error(web("a", "file:///etc/passwd"), "http")
  expect_error(web("a", "ftp://x.example/"), "http")
})

test_that("web() rejects an invalid regex pattern", {
  expect_error(web("a", "https://x.example/", pattern = "["), "regular expression")
})

test_that("local_dir() builds a source spec that takes every file by default", {
  s <- local_dir("notes", "/tmp/docs")
  expect_s3_class(s, "ragdoc_source")
  expect_identical(s$name, "notes")
  expect_identical(s$path, "/tmp/docs")
  expect_null(s$file_pattern)  # default: every file under path
  expect_true(s$recursive)
  expect_identical(s$type, "local")
})

test_that("local_dir() accepts a custom pattern and recursive flag", {
  s <- local_dir("notes", "/tmp/docs", pattern = "\\.(md|markdown)$", recursive = FALSE)
  expect_identical(s$file_pattern, "\\.(md|markdown)$")
  expect_false(s$recursive)
})

test_that("local_dir() allows pattern = NULL to take every file", {
  s <- local_dir("notes", "/tmp/docs", pattern = NULL)
  expect_null(s$file_pattern)
})

test_that("local_dir() rejects empty or non-string name and path", {
  expect_error(local_dir("", "/tmp/docs"), "name")
  expect_error(local_dir("ok", ""), "path")
  expect_error(local_dir(1, "/tmp/docs"), "name")
})

test_that("local_dir() rejects an invalid regex pattern", {
  expect_error(local_dir("a", "/tmp/docs", pattern = "["), "regular expression")
})

test_that("local_dir() rejects a non-logical recursive", {
  expect_error(local_dir("a", "/tmp/docs", recursive = "yes"), "recursive")
  expect_error(local_dir("a", "/tmp/docs", recursive = NA), "recursive")
})

test_that("sources() collects sources and preserves order", {
  spec <- sources(
    web("a", "https://a.example/"),
    web("b", "https://b.example/")
  )
  expect_s3_class(spec, "ragdoc_sources")
  expect_length(spec, 2)
  expect_identical(vapply(spec, `[[`, character(1), "name"), c("a", "b"))
})

test_that("sources() mixes web and local sources", {
  spec <- sources(
    web("api", "https://api.example/"),
    local_dir("notes", "/tmp/docs")
  )
  expect_length(spec, 2)
  expect_identical(vapply(spec, `[[`, character(1), "type"), c("web", "local"))
})

test_that("sources() requires at least one source", {
  expect_error(sources(), "at least one source")
})

test_that("sources() rejects non-source arguments", {
  expect_error(sources(list(name = "x")), "must be a source")
})

test_that("sources() rejects duplicate names", {
  expect_error(
    sources(web("dup", "https://a.example/"), web("dup", "https://b.example/")),
    "unique"
  )
})
