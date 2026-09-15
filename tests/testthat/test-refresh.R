# Gold end-to-end test of incremental refresh against the REAL ragnar + DuckDB
# stack, with a fake, self-contained counting embedder so it needs no network or
# API key. This is the test that would catch a regression in the dedup that the
# whole feature exists to provide -- the mocked dispatch tests in test-build.R
# cannot see it.
#
# Counting embeds across a refresh is the trick: ragnar serializes the embed
# function into the store, runs it in parallel workers, and deserializes it on
# connect. A closure that increments a counter env would lose that env across
# any of those hops. So we bake the count-file path into the function body as a
# literal (via substitute()), making the embedder fully self-contained.

counting_embed <- function(count_file) {
  f <- function(x) {
    cat(length(x), "\n", file = PATH, append = TRUE)
    matrix(stats::rnorm(length(x) * 8L), nrow = length(x))
  }
  body(f) <- do.call(substitute, list(body(f), list(PATH = count_file)))
  environment(f) <- globalenv()
  f
}

test_that("refresh re-embeds nothing on an unchanged corpus, only the edited file after a change", {
  skip_on_cran()
  skip_if_not_installed("mirai")

  count_file <- withr::local_tempfile()
  embeds <- function() {
    if (!file.exists(count_file)) return(0L)
    sum(as.integer(readLines(count_file)))
  }

  dir <- withr::local_tempdir()
  writeLines(c("# Testing", "Use testthat to write unit tests for your functions."),
             file.path(dir, "a.md"))
  writeLines(c("# fit", "fit <- lm(y ~ x, data = d)", "summary(fit)"),
             file.path(dir, "b.R"))

  path <- withr::local_tempfile(fileext = ".duckdb")
  spec <- sources(local_dir("notes", dir))

  suppressMessages(build_store(spec, path, embed = counting_embed(count_file)))
  n_initial <- embeds()
  expect_gt(n_initial, 0)  # the initial build embedded something

  # Refresh over the unchanged corpus: the whole point is that this costs zero
  # additional embeds.
  suppressMessages(refresh_store(spec, path))
  expect_identical(embeds(), n_initial)

  # Edit one file; only that file is re-embedded, not the whole corpus.
  writeLines(c("# Testing", "Completely different text about vapply and purrr map over a list."),
             file.path(dir, "a.md"))
  suppressMessages(refresh_store(spec, path))
  expect_gt(embeds(), n_initial)
})
