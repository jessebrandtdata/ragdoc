# Fetch one page and return its markdown chunks. A thin seam over ragnar's
# read-and-chunk pair, kept as its own function so the per-item failure handling
# in ingest_items() has a single point to fail at (and to mock in tests).
read_page_chunks <- function(page) {
  ragnar::markdown_chunk(ragnar::read_as_markdown(page))
}

# Read one local file and return its markdown chunks. The local-source twin of
# read_page_chunks(): same shape, its own seam so per-file failures are isolated
# and mockable. ragnar::read_as_markdown() handles a file path as readily as a URL.
read_file_chunks <- function(file) {
  ragnar::markdown_chunk(ragnar::read_as_markdown(file))
}

# Insert the chunks of every item into `store`, with per-item failure isolation.
# Shared by every source type: discovery (which links/files to ingest) differs by
# type and lives in the ingest_* functions, but the resilient insert-and-tally
# loop is identical, so it lives here once.
#
# `items`   character vector of locators (URLs for web, file paths for local);
#           each is recorded verbatim in the chunk `url` column for citation.
# `read_one` function mapping one locator to a chunk data frame.
# `unit`    plural noun for the summary line ("pages", "files").
#
# Returns TRUE if every item succeeded, FALSE if any was skipped. An item that
# yields zero chunks is counted "empty" (not a failure).
ingest_items <- function(store, source, items, read_one, unit) {
  ok <- 0L
  empty <- 0L
  skipped <- character()
  for (item in items) {
    # tryCatch yields TRUE (ingested), NA (no chunks), or FALSE (error).
    outcome <- tryCatch({
      chunks <- read_one(item)
      if (nrow(chunks) == 0L) {
        message("  empty ", item)
        NA
      } else {
        chunks$source <- source$name
        chunks$url <- item
        ragnar::ragnar_store_insert(store, chunks)
        TRUE
      }
    }, error = function(e) {
      message("  skip ", item, ": ", conditionMessage(e))
      FALSE
    })
    if (isTRUE(outcome)) {
      ok <- ok + 1L
    } else if (is.na(outcome)) {
      empty <- empty + 1L
    } else {
      skipped <- c(skipped, item)
    }
  }
  message(sprintf("%s: %d/%d %s ingested, %d empty, %d skipped",
                  source$name, ok, length(items), unit, empty, length(skipped)))
  length(skipped) == 0L
}

#' Ingest one source into the store
#'
#' Dispatches on `source$type` to the per-type ingestion routine. Each routine
#' discovers what to ingest (links for web, files for local) and hands the work
#' to the shared `ingest_items()` loop.
#'
#' @param store A ragnar store opened for writing.
#' @param source A single `ragdoc_source` (see [web()], [local_dir()]).
#'
#' @return `TRUE` if every item succeeded, `FALSE` if any was skipped.
#' @keywords internal
#' @noRd
ingest_source <- function(store, source) {
  switch(source$type,
    web   = ingest_web(store, source),
    local = ingest_local(store, source),
    stop("unknown source type: ", source$type)
  )
}

# Crawl a web source's root_url, filter by crawl_pattern, ingest each page.
# Per-page errors are logged and skipped, so one broken page does not abort the
# crawl. Returns FALSE if link discovery fails or nothing matches.
ingest_web <- function(store, source) {
  links <- tryCatch(
    ragnar::ragnar_find_links(source$root_url),
    error = function(e) {
      message(sprintf("%s: link discovery failed -- %s",
                      source$name, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(links)) return(FALSE)
  if (!is.null(source$crawl_pattern)) {
    links <- grep(source$crawl_pattern, links, value = TRUE)
  }
  links <- unique(links)
  if (!length(links)) {
    hint <- if (is.null(source$crawl_pattern)) {
      "no links found -- check `root_url`"
    } else {
      "0 links matched `pattern` -- check the pattern"
    }
    message(sprintf("%s: %s", source$name, hint))
    return(FALSE)
  }
  ingest_items(store, source, links, read_page_chunks, "pages")
}

# List a local source's directory, filter by file_pattern, ingest each file.
# Per-file errors are logged and skipped. Returns FALSE if the directory is
# missing or nothing matches.
ingest_local <- function(store, source) {
  if (!dir.exists(source$path)) {
    message(sprintf("%s: directory does not exist -- %s",
                    source$name, source$path))
    return(FALSE)
  }
  files <- list.files(
    source$path,
    pattern = source$file_pattern,
    recursive = source$recursive,
    full.names = TRUE
  )
  files <- unique(files)
  if (!length(files)) {
    hint <- if (is.null(source$file_pattern)) {
      "no files found -- check `path`"
    } else {
      "no files matched `pattern` -- check the pattern"
    }
    message(sprintf("%s: %s", source$name, hint))
    return(FALSE)
  }
  ingest_items(store, source, files, read_file_chunks, "files")
}

#' Build a RAG store from a sources spec
#'
#' Creates a fresh DuckDB store, ingests every source in `sources`, and builds
#' the hybrid retrieval index. Per-source failure is isolated: one bad source
#' logs an error and is skipped without aborting the rest, so a partial store is
#' still usable.
#'
#' By default documents are embedded with OpenAI's `text-embedding-3-small`,
#' which requires `OPENAI_API_KEY` in the environment. Pass `embed` to use a
#' different embedder (any function ragnar accepts).
#'
#' @param sources A sources spec from [sources()].
#' @param path Path to the DuckDB store to create.
#' @param embed Optional embedding function. When `NULL` (default), uses OpenAI
#'   `text-embedding-3-small` and requires `OPENAI_API_KEY`.
#' @param overwrite Overwrite an existing store at `path`? Defaults to `FALSE`,
#'   so an existing store is left intact unless you explicitly opt in --
#'   rebuilding discards a store that may represent real crawl time and API
#'   spend.
#'
#' @return Invisibly, `TRUE` if every source ingested cleanly, `FALSE`
#'   otherwise. The store and its index are built either way.
#' @seealso [sources()], [connect_store()], [search_docs()]
#' @export
#' @examples
#' \dontrun{
#' src <- sources(web("handbook", "https://docs.example.com/", pattern = "\\.html$"))
#' build_store(src, "handbook.duckdb")
#' }
build_store <- function(sources, path, embed = NULL, overwrite = FALSE) {
  if (file.exists(path) && !overwrite) {
    stop("a store already exists at ", path,
         " -- pass `overwrite = TRUE` to rebuild it")
  }
  if (is.null(embed)) {
    if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
      stop("OPENAI_API_KEY not set -- add it to ~/.Renviron, or pass `embed=`")
    }
    embed <- function(x) ragnar::embed_openai(x, model = "text-embedding-3-small")
  } else if (!is.function(embed)) {
    stop("`embed` must be a function, or NULL to use the OpenAI default")
  }
  store <- ragnar::ragnar_store_create(
    path,
    embed = embed,
    extra_cols = data.frame(source = character(), url = character()),
    overwrite = overwrite
  )
  all_ok <- TRUE
  for (s in sources) {
    src_ok <- tryCatch(
      ingest_source(store, s),
      error = function(e) {
        message("source failed: ", s$name, " -- ", conditionMessage(e))
        FALSE
      }
    )
    all_ok <- all_ok && isTRUE(src_ok)
  }
  ragnar::ragnar_store_build_index(store)
  invisible(all_ok)
}
