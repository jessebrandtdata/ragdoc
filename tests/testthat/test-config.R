test_that("load_sources parses web and local entries into a sources spec", {
  skip_if_not_installed("yaml")
  yml <- withr::local_tempfile(fileext = ".yml")
  writeLines(c(
    "- name: handbook",
    "  root_url: https://docs.example.com/",
    "  crawl_pattern: \"\\\\.html$\"",
    "- name: notes",
    "  path: /tmp/docs",
    "  recursive: false"
  ), yml)
  spec <- load_sources(yml)
  expect_s3_class(spec, "ragdoc_sources")
  expect_length(spec, 2)
  expect_identical(vapply(spec, `[[`, character(1), "type"), c("web", "local"))
  expect_identical(spec[[1]]$crawl_pattern, "\\.html$")
  expect_identical(spec[[2]]$path, "/tmp/docs")
  expect_false(spec[[2]]$recursive)
})

test_that("load_sources leaves an unset local file_pattern at the constructor default", {
  skip_if_not_installed("yaml")
  yml <- withr::local_tempfile(fileext = ".yml")
  writeLines(c("- name: notes", "  path: /tmp/docs"), yml)
  spec <- load_sources(yml)
  expect_null(spec[[1]]$file_pattern)  # local_dir() default (every file)
})

test_that("load_sources errors on a source missing name, or lacking both url and path", {
  skip_if_not_installed("yaml")
  yml <- withr::local_tempfile(fileext = ".yml")
  writeLines("- root_url: https://x.example/", yml)
  expect_error(load_sources(yml), "missing a `name`")
  writeLines("- name: x", yml)
  expect_error(load_sources(yml), "either `root_url`")
})

test_that("load_sources errors on a missing file", {
  expect_error(load_sources(tempfile(fileext = ".yml")), "not found")
})

test_that("load_sources treats an entry with both root_url and path as a web source", {
  skip_if_not_installed("yaml")
  yml <- withr::local_tempfile(fileext = ".yml")
  writeLines(c(
    "- name: both",
    "  root_url: https://x.example/",
    "  path: /tmp/docs"
  ), yml)
  spec <- load_sources(yml)
  expect_identical(spec[[1]]$type, "web")   # root_url is checked before path
  expect_identical(spec[[1]]$root_url, "https://x.example/")
})

test_that("load_sources forwards file_pattern and recursive = true to local_dir", {
  skip_if_not_installed("yaml")
  yml <- withr::local_tempfile(fileext = ".yml")
  writeLines(c(
    "- name: notes",
    "  path: /tmp/docs",
    "  file_pattern: \"\\\\.md$\"",
    "  recursive: true"
  ), yml)
  spec <- load_sources(yml)
  expect_identical(spec[[1]]$file_pattern, "\\.md$")
  expect_true(spec[[1]]$recursive)
})

test_that("load_sources errors on an empty or malformed file", {
  skip_if_not_installed("yaml")
  yml <- withr::local_tempfile(fileext = ".yml")
  writeLines("", yml)  # parses to NULL -> neither a list nor length > 0
  expect_error(load_sources(yml), "empty or malformed")
})
