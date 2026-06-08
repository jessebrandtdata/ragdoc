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

test_that("sources() collects sources and preserves order", {
  spec <- sources(
    web("a", "https://a.example/"),
    web("b", "https://b.example/")
  )
  expect_s3_class(spec, "ragdoc_sources")
  expect_length(spec, 2)
  expect_identical(vapply(spec, `[[`, character(1), "name"), c("a", "b"))
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
