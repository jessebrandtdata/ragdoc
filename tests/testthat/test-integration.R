# Integration test against a real (tiny, local, network-free) ragnar store.
# Mocks can prove the source filter is *forwarded*; only a real store proves it
# actually *restricts* retrieval -- the point of pushing the filter into ragnar.
# A deterministic fake embedder stands in for OpenAI, so no network/API is used.

fake_embed <- function(x) {
  d <- 8L
  m <- matrix(0, nrow = length(x), ncol = d)
  for (i in seq_along(x)) {
    codes <- utf8ToInt(x[i])
    for (j in seq_len(d)) m[i, j] <- sum(codes[seq(j, length(codes), by = d)], na.rm = TRUE)
  }
  m / (sqrt(rowSums(m^2)) + 1e-9)
}

test_that("search_docs restricts retrieval to a single source end-to-end", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  writeLines(c("# Alpha", "alpha apple content about widgets", "",
               "## More", "alpha avocado section here"), file.path(dir, "a.md"))
  writeLines(c("# Beta", "beta banana content about gadgets", "",
               "## More", "gamma grape section here"), file.path(dir, "b.md"))

  path <- withr::local_tempfile(fileext = ".duckdb")
  store <- ragnar::ragnar_store_create(
    path, embed = fake_embed, embedding_size = 8L,
    extra_cols = data.frame(source = character(), url = character()),
    overwrite = TRUE
  )
  for (f in c("a", "b")) {
    ch <- ragnar::markdown_chunk(ragnar::read_as_markdown(file.path(dir, paste0(f, ".md"))))
    ch$source <- f
    ch$url <- paste0("https://", f, ".example/", f, ".md")
    ragnar::ragnar_store_insert(store, ch)
  }
  ragnar::ragnar_store_build_index(store)

  store <- connect_store(path)

  # Filtered to "a": every cited passage must come from source "a", none from "b".
  out_a <- search_docs(store, "alpha section widgets", n = 10, source = "a")
  expect_match(out_a, "\\[a \u00b7")
  expect_no_match(out_a, "\\[b \u00b7")

  # Sanity: unfiltered search can reach source "b" at all (so the filter above
  # is doing real work, not just reflecting an empty corpus).
  out_all <- search_docs(store, "beta gadgets section", n = 10)
  expect_match(out_all, "\\[b \u00b7")
})

test_that("build_store indexes a local directory end-to-end", {
  skip_on_cran()
  # build_store() lets ragnar infer embedding size by probing embed("foo"), so the
  # embedder must handle any-length input (unlike the fake_embed above, which is
  # only ever called on chunk text >= 8 chars). This one is length-agnostic.
  probe_safe_embed <- function(x) {
    d <- 8L
    m <- t(vapply(x, function(s) {
      codes <- utf8ToInt(s)
      v <- numeric(d)
      for (k in seq_along(codes)) {
        idx <- ((k - 1L) %% d) + 1L
        v[idx] <- v[idx] + codes[k]
      }
      v
    }, numeric(d)))
    m / (sqrt(rowSums(m^2)) + 1e-9)
  }

  docs <- withr::local_tempdir()
  writeLines(c("# Widgets", "alpha apple content about widgets", "",
               "## More", "alpha avocado section here"), file.path(docs, "widgets.md"))
  sub <- file.path(docs, "deep")
  dir.create(sub)
  writeLines(c("# Gadgets", "beta banana content about gadgets", "",
               "## More", "gamma grape section here"), file.path(sub, "gadgets.md"))
  writeLines("ignore me, not markdown", file.path(docs, "notes.txt"))

  path <- withr::local_tempfile(fileext = ".duckdb")
  spec <- sources(local_dir("docs", docs, pattern = "\\.md$"))
  ok <- suppressMessages(build_store(spec, path, embed = probe_safe_embed))
  expect_true(ok)

  store <- connect_store(path)

  # The recursive .md walk reached both files; the .txt was excluded by pattern.
  out <- search_docs(store, "widgets gadgets section", n = 10)
  expect_match(out, "widgets\\.md")
  expect_match(out, "gadgets\\.md")
  expect_no_match(out, "notes\\.txt")
  # Citations are tagged with the source name.
  expect_match(out, "\\[docs \u00b7")
})
