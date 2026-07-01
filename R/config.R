#' Load documentation sources from a YAML file
#'
#' Reads a `sources.yml` file into a [sources()] spec, so a corpus can be kept
#' declaratively in one config file instead of in R code. Each top-level YAML
#' entry becomes one source: an entry with a `root_url` is a [web()] source, and
#' an entry with a `path` is a [local_dir()] source.
#'
#' The recognized fields mirror the constructor arguments:
#'
#' ```yaml
#' - name: handbook              # web source
#'   root_url: https://docs.example.com/
#'   crawl_pattern: "\\.html$"   # optional, -> web(pattern=)
#' - name: notes                 # local source
#'   path: ~/project/docs
#'   file_pattern: "\\.md$"      # optional, -> local_dir(pattern=)
#'   recursive: false            # optional, -> local_dir(recursive=)
#' ```
#'
#' Requires the \pkg{yaml} package.
#'
#' @param path Path to the YAML file.
#'
#' @return A `ragdoc_sources` object (see [sources()]), ready for [build_store()].
#' @seealso [sources()], [web()], [local_dir()], [build_store()]
#' @export
#' @examples
#' \dontrun{
#' src <- load_sources("sources.yml")
#' build_store(src, "docs.duckdb")
#' }
load_sources <- function(path) {
  if (!file.exists(path)) {
    stop("sources file not found: ", path)
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("load_sources() needs the 'yaml' package; install it first")
  }
  raw <- yaml::read_yaml(path)
  if (!is.list(raw) || !length(raw)) {
    stop("sources file is empty or malformed: ", path)
  }
  srcs <- lapply(seq_along(raw), function(i) {
    s <- raw[[i]]
    if (!is.list(s) || is.null(s$name) || !nzchar(s$name)) {
      stop(sprintf("source #%d is missing a `name`", i))
    }
    if (!is.null(s$root_url)) {
      web(s$name, s$root_url, pattern = s$crawl_pattern)
    } else if (!is.null(s$path)) {
      args <- list(name = s$name, path = s$path)
      if ("file_pattern" %in% names(s)) args$pattern <- s$file_pattern
      if (!is.null(s$recursive)) args$recursive <- s$recursive
      do.call(local_dir, args)
    } else {
      stop(sprintf("source '%s' must have either `root_url` (web) or `path` (local)",
                   s$name))
    }
  })
  do.call(sources, srcs)
}
