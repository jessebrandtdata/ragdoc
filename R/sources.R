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
#' @param root_url The site to crawl. Must be an `http://` or `https://` URL.
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
  if (!grepl("^https?://", root_url)) {
    stop("`root_url` must be an http:// or https:// URL: ", root_url)
  }
  if (!is.null(pattern)) {
    if (!is_string(pattern)) {
      stop("`pattern` must be a string or NULL")
    }
    ok <- tryCatch({
      suppressWarnings(grepl(pattern, ""))
      TRUE
    }, error = function(e) FALSE)
    if (!ok) stop("`pattern` is not a valid regular expression: ", pattern)
  }
  structure(
    list(name = name, root_url = root_url, crawl_pattern = pattern, type = "web"),
    class = "ragdoc_source"
  )
}

#' Declare a local directory source
#'
#' Describes a directory of files to ingest into a store. [build_store()] lists
#' the files under `path` whose names match `pattern`, converts each to markdown,
#' chunks it, and records `name` on every chunk so results can be cited and
#' filtered by source. The file's path is recorded as its citation locator (the
#' `url` column), so retrieved passages point back at the file they came from.
#'
#' Files are read by extension: plain text and Markdown (`.md`, `.txt`) verbatim,
#' source code (`.R`, `.py`, `.qmd`, `.sql`, …) wrapped in a language-fenced
#' block, and everything else (`.pdf`, `.docx`, `.pptx`, `.html`, `.ipynb`, …)
#' converted with MarkItDown via [ragnar::read_as_markdown()], which needs a
#' Python environment with `markitdown` installed. Files over 25 MB are skipped
#' at build time (large PDFs can exhaust memory).
#'
#' @param name Short source label, recorded on every chunk and usable as the
#'   `source` filter in [search_docs()]. Must be unique within a [sources()]
#'   spec.
#' @param path Path to the directory to ingest. The directory is read at build
#'   time, so it need not exist when the source is declared.
#' @param pattern Regular expression matched against file *names* (not full
#'   paths); only matching files are ingested. Defaults to `NULL`, which takes
#'   every file under `path`. Pass a pattern to narrow, e.g. `"\\.md$"`.
#' @param recursive Descend into subdirectories? Defaults to `TRUE`.
#'
#' @return A `ragdoc_source` object, to be passed to [sources()].
#' @seealso [sources()], [web()], [build_store()]
#' @export
#' @examples
#' local_dir("notes", "~/project/docs")
#' local_dir("notes", "~/project/docs", pattern = "\\.(md|qmd|pdf)$")
local_dir <- function(name, path, pattern = NULL, recursive = TRUE) {
  if (!is_string(name) || !nzchar(name)) {
    stop("`name` must be a non-empty string")
  }
  if (!is_string(path) || !nzchar(path)) {
    stop("`path` must be a non-empty string")
  }
  if (!is.null(pattern)) {
    if (!is_string(pattern)) {
      stop("`pattern` must be a string or NULL")
    }
    ok <- tryCatch({
      suppressWarnings(grepl(pattern, ""))
      TRUE
    }, error = function(e) FALSE)
    if (!ok) stop("`pattern` is not a valid regular expression: ", pattern)
  }
  if (!is.logical(recursive) || length(recursive) != 1L || is.na(recursive)) {
    stop("`recursive` must be a single TRUE or FALSE")
  }
  structure(
    list(name = name, path = path, file_pattern = pattern,
         recursive = recursive, type = "local"),
    class = "ragdoc_source"
  )
}

#' Collect documentation sources into a build spec
#'
#' Bundles one or more sources ([web()] or [local_dir()]) into the spec that
#' [build_store()] ingests. Source names must be unique so retrieved chunks can
#' be attributed and filtered unambiguously.
#'
#' @param ... One or more `ragdoc_source` objects, e.g. from [web()] or
#'   [local_dir()].
#'
#' @return A `ragdoc_sources` object: a list of sources to build from.
#' @seealso [web()], [local_dir()], [build_store()]
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
