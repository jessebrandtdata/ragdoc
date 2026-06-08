# Fetch one page and return its markdown chunks. A thin seam over ragnar's
# read-and-chunk pair, kept as its own function so the per-page failure handling
# in ingest_source() has a single point to fail at (and to mock in tests).
read_page_chunks <- function(page) {
  ragnar::markdown_chunk(ragnar::read_as_markdown(page))
}

#' Ingest one web source into the store
#'
#' Crawls a single source's `root_url`, fetches each matching page, converts it
#' to markdown, chunks it, and inserts the chunks into `store`. Per-page errors
#' are logged and skipped (not fatal), so one broken page does not abort the
#' crawl.
#'
#' @param store A ragnar store opened for writing.
#' @param source A single `ragdoc_source` (see [web()]).
#'
#' @return `TRUE` if every page succeeded, `FALSE` if any page was skipped.
#' @keywords internal
#' @noRd
ingest_source <- function(store, source) {
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
  ok <- 0L
  empty <- 0L
  skipped <- character()
  for (page in links) {
    # tryCatch yields TRUE (ingested), NA (page had no chunks), or FALSE (error).
    outcome <- tryCatch({
      chunks <- read_page_chunks(page)
      if (nrow(chunks) == 0L) {
        message("  empty ", page)
        NA
      } else {
        chunks$source <- source$name
        chunks$url <- page
        ragnar::ragnar_store_insert(store, chunks)
        TRUE
      }
    }, error = function(e) {
      message("  skip ", page, ": ", conditionMessage(e))
      FALSE
    })
    if (isTRUE(outcome)) {
      ok <- ok + 1L
    } else if (is.na(outcome)) {
      empty <- empty + 1L
    } else {
      skipped <- c(skipped, page)
    }
  }
  message(sprintf("%s: %d/%d pages ingested, %d empty, %d skipped",
                  source$name, ok, length(links), empty, length(skipped)))
  length(skipped) == 0L
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
