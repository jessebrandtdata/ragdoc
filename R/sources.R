is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

#' Declare a web documentation source
#'
#' Describes one site to crawl and ingest into a store. [build_store()] visits
#' `root_url`, follows the links found there (optionally filtered by `pattern`),
#' converts each page to markdown, chunks it, and records `name` on every chunk
#' so results can be cited and filtered by source.
#'
#' @param name Short source label, recorded on every chunk and usable as the
#'   `source` filter in [search_docs()]. Must be unique within a [sources()]
#'   spec.
#' @param root_url The site to crawl.
#' @param pattern Optional regular expression; only links matching it are
#'   ingested. Use it to stay within one book or section and skip nav/asset
#'   links, e.g. `"^https://docs\\.example\\.com/[^#]+\\.html$"`.
#'
#' @return A `ragdoc_source` object, to be passed to [sources()].
#' @seealso [sources()], [build_store()]
#' @export
#' @examples
#' web("handbook", "https://docs.example.com/", pattern = "\\.html$")
web <- function(name, root_url, pattern = NULL) {
  if (!is_string(name) || !nzchar(name)) {
    stop("`name` must be a non-empty string")
  }
  if (!is_string(root_url) || !nzchar(root_url)) {
    stop("`root_url` must be a non-empty string")
  }
  if (!is.null(pattern) && !is_string(pattern)) {
    stop("`pattern` must be a string or NULL")
  }
  structure(
    list(name = name, root_url = root_url, crawl_pattern = pattern, type = "web"),
    class = "ragdoc_source"
  )
}

#' Collect documentation sources into a build spec
#'
#' Bundles one or more sources (currently [web()] sources) into the spec that
#' [build_store()] ingests. Source names must be unique so retrieved chunks can
#' be attributed and filtered unambiguously.
#'
#' @param ... One or more `ragdoc_source` objects, e.g. from [web()].
#'
#' @return A `ragdoc_sources` object: a list of sources to build from.
#' @seealso [web()], [build_store()]
#' @export
#' @examples
#' sources(
#'   web("handbook", "https://docs.example.com/", pattern = "\\.html$"),
#'   web("api",      "https://api.example.com/",  pattern = "\\.html$")
#' )
sources <- function(...) {
  srcs <- list(...)
  if (!length(srcs)) {
    stop("supply at least one source (see `web()`)")
  }
  ok <- vapply(srcs, inherits, logical(1), what = "ragdoc_source")
  if (!all(ok)) {
    stop("every argument to `sources()` must be a source, e.g. from `web()`")
  }
  nms <- vapply(srcs, function(s) s$name, character(1))
  if (anyDuplicated(nms)) {
    dups <- unique(nms[duplicated(nms)])
    stop("source names must be unique; duplicated: ", paste(dups, collapse = ", "))
  }
  structure(srcs, class = "ragdoc_sources")
}
