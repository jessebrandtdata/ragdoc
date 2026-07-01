# Local files larger than this are skipped at ingest time. Large PDFs/EPUBs make
# MarkItDown load every page image into memory and can OOM a small host; the
# cap is a guard against a stray big file derailing a build. Web pages are not
# size-capped this way.
.MAX_INGEST_BYTES <- 25 * 1024 * 1024

# --- per-source `prepare` functions -----------------------------------------
#
# ragnar::ragnar_store_ingest() runs `prepare` in parallel worker processes, so
# each prepare function must be SELF-CONTAINED: it may call base R and ragnar
# (loaded in the workers) but must not reference ragdoc's own internals. The
# factories below capture everything they need (the source name, the extension
# tables) by value, so the returned closure serializes cleanly to a worker.
#
# Every prepare returns a ragnar `MarkdownDocumentChunks` object with two extra
# columns tagged on: `source` (the source label) and `url` (the page URL or file
# path). The document `origin` is set to that same locator so incremental
# refresh can tell which items are unchanged.

# Web: read the page as markdown, chunk it, tag it.
web_prepare_fn <- function(source_name) {
  force(source_name)
  function(path) {
    chunks <- ragnar::markdown_chunk(ragnar::read_as_markdown(path))
    chunks$source <- source_name
    chunks$url <- path
    chunks
  }
}

# Local: dispatch on file extension. Plain text and markdown are read verbatim;
# source code is wrapped in a language-fenced block (so retrieval sees it as
# code, with a language hint); everything else (pdf, docx, pptx, html, ipynb, …)
# goes through ragnar::read_as_markdown(), which uses MarkItDown under the hood.
# For the text/code paths we build the MarkdownDocument ourselves so its `origin`
# is the file path -- read_as_markdown() sets `origin` on its own.
local_prepare_fn <- function(source_name) {
  force(source_name)
  text_exts <- c("md", "txt")
  code_exts <- c("r", "py", "sql", "qmd", "rmd", "rnw", "sh", "js", "ts",
                 "java", "c", "cpp", "h", "hpp", "rb", "go", "rs", "do", "tex")
  function(path) {
    ext <- tolower(tools::file_ext(path))
    doc <- if (ext %in% text_exts) {
      txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      ragnar::MarkdownDocument(txt, origin = path)
    } else if (ext %in% code_exts) {
      txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      lang <- switch(ext,
        r = "r", py = "python", sql = "sql", sh = "bash", js = "javascript",
        ts = "typescript", qmd = "markdown", rmd = "markdown", rnw = "noweb", "")
      ragnar::MarkdownDocument(paste0("```", lang, "\n", txt, "\n```\n"), origin = path)
    } else {
      ragnar::read_as_markdown(path)
    }
    chunks <- ragnar::markdown_chunk(doc)
    chunks$source <- source_name
    chunks$url <- path
    chunks
  }
}

# --- per-source discovery + ingest ------------------------------------------
#
# Each ingest_* function discovers what to ingest (links for web, files for
# local) and hands the list plus the matching prepare function to ragnar's
# ingester, which does the read -> chunk -> embed -> insert work with per-item
# error isolation and origin-level dedup. `build_index = FALSE` because the
# index is built once after every source is in.

# Dispatch on source$type.
ingest_source <- function(store, source) {
  switch(source$type,
    web   = ingest_web(store, source),
    local = ingest_local(store, source),
    stop("unknown source type: ", source$type)
  )
}

# Crawl a web source, filter links by crawl_pattern, ingest each page. Returns
# FALSE if link discovery fails or nothing matches, TRUE once handed to ragnar.
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
  message(sprintf("%s: ingesting %d pages", source$name, length(links)))
  ragnar::ragnar_store_ingest(store, links, build_index = FALSE,
                              prepare = web_prepare_fn(source$name))
  TRUE
}

# List a local source's directory, drop oversize files, ingest the rest. Returns
# FALSE if the directory is missing or nothing is left to ingest, TRUE once
# handed to ragnar.
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
  files <- unique(files[!dir.exists(files)])
  if (!length(files)) {
    hint <- if (is.null(source$file_pattern)) {
      "no files found -- check `path`"
    } else {
      "no files matched `pattern` -- check the pattern"
    }
    message(sprintf("%s: %s", source$name, hint))
    return(FALSE)
  }
  sizes <- file.info(files)$size
  oversize <- !is.na(sizes) & sizes > .MAX_INGEST_BYTES
  if (any(oversize)) {
    message(sprintf("%s: skipping %d file(s) over %.0f MB",
                    source$name, sum(oversize), .MAX_INGEST_BYTES / 1024 / 1024))
    files <- files[!oversize]
  }
  if (!length(files)) return(FALSE)
  message(sprintf("%s: ingesting %d files", source$name, length(files)))
  ragnar::ragnar_store_ingest(store, files, build_index = FALSE,
                              prepare = local_prepare_fn(source$name))
  TRUE
}

# Resolve the `embed` argument shared by build_store(). NULL -> OpenAI default
# (requires OPENAI_API_KEY); a function is used as-is; anything else errors.
resolve_embed <- function(embed) {
  if (is.null(embed)) {
    if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
      stop("OPENAI_API_KEY not set -- add it to ~/.Renviron, or pass `embed=`")
    }
    function(x) ragnar::embed_openai(x, model = "text-embedding-3-small")
  } else if (!is.function(embed)) {
    stop("`embed` must be a function, or NULL to use the OpenAI default")
  } else {
    embed
  }
}

# Building and refreshing ingest through ragnar::ragnar_store_ingest(), which
# runs its workers on mirai. Check for it up front so a missing dependency is a
# clear message before any store is created or opened, not a failure mid-ingest.
check_ingest_deps <- function() {
  if (!requireNamespace("mirai", quietly = TRUE)) {
    stop("building or refreshing a store needs the 'mirai' package; install it first")
  }
}

# Ingest every source into an open store, isolating per-source failure: one bad
# source logs an error and is skipped without aborting the rest. Returns TRUE if
# every source ingested cleanly.
ingest_all <- function(store, sources) {
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
  all_ok
}

#' Build a RAG store from a sources spec
#'
#' Creates a fresh DuckDB store, ingests every source in `sources`, and builds
#' the hybrid retrieval index. Per-source failure is isolated: one bad source
#' logs an error and is skipped without aborting the rest, so a partial store is
#' still usable. Once built, update it cheaply in place with [refresh_store()]
#' instead of rebuilding from scratch.
#'
#' By default documents are embedded with OpenAI's `text-embedding-3-small`,
#' which requires `OPENAI_API_KEY` in the environment. Pass `embed` to use a
#' different embedder (any function ragnar accepts); ragnar records the embedder
#' in the store, so [refresh_store()] reuses it automatically.
#'
#' Ingestion runs in parallel worker processes via \pkg{mirai}, and non-text
#' local files (pdf, docx, …) are converted with MarkItDown through
#' [ragnar::read_as_markdown()], which needs a Python environment with
#' `markitdown` installed.
#'
#' @param sources A sources spec from [sources()].
#' @param path Path to the DuckDB store to create.
#' @param embed Optional embedding function. When `NULL` (default), uses OpenAI
#'   `text-embedding-3-small` and requires `OPENAI_API_KEY`.
#' @param overwrite Overwrite an existing store at `path`? Defaults to `FALSE`,
#'   so an existing store is left intact unless you explicitly opt in --
#'   rebuilding discards a store that may represent real crawl time and API
#'   spend. To *update* an existing store rather than replace it, use
#'   [refresh_store()].
#'
#' @return Invisibly, `TRUE` if every source ingested cleanly, `FALSE`
#'   otherwise. The store and its index are built either way.
#' @seealso [sources()], [refresh_store()], [connect_store()], [search_docs()]
#' @export
#' @examples
#' \dontrun{
#' src <- sources(web("handbook", "https://docs.example.com/", pattern = "\\.html$"))
#' build_store(src, "handbook.duckdb")
#' }
build_store <- function(sources, path, embed = NULL, overwrite = FALSE) {
  if (file.exists(path) && !overwrite) {
    stop("a store already exists at ", path,
         " -- pass `overwrite = TRUE` to rebuild it, or use `refresh_store()`",
         " to update it in place")
  }
  check_ingest_deps()
  embed <- resolve_embed(embed)
  store <- ragnar::ragnar_store_create(
    path,
    embed = embed,
    extra_cols = data.frame(source = character(), url = character()),
    overwrite = overwrite
  )
  all_ok <- ingest_all(store, sources)
  ragnar::ragnar_store_build_index(store)
  invisible(all_ok)
}

#' Refresh an existing store in place
#'
#' Re-ingests `sources` into the store already at `path`, then rebuilds the
#' index. This is the cheap update path: ragnar skips any page or file whose
#' content is unchanged and re-embeds only what is new or edited, so a refresh
#' over a mostly-stable corpus costs little. Contrast [build_store()], which
#' re-embeds everything from scratch.
#'
#' The store keeps the embedder it was built with, so `refresh_store()` takes no
#' `embed` argument -- it reuses the original model automatically.
#'
#' Refresh is add-and-update only: it never *removes* anything. A page dropped or
#' renamed upstream lingers in the store with stale content and a dead citation
#' URL. Reach for a full rebuild ([build_store()] with `overwrite = TRUE`) when
#' pages are removed or renamed, when the site is restructured, or when you
#' change the chunking, the embedder, or a source's `name`.
#'
#' @param sources A sources spec from [sources()] -- typically the same one you
#'   built with.
#' @param path Path to an existing store created by [build_store()].
#'
#' @return Invisibly, `TRUE` if every source ingested cleanly, `FALSE`
#'   otherwise. The index is rebuilt either way.
#' @seealso [build_store()], [sources()]
#' @export
#' @examples
#' \dontrun{
#' src <- sources(web("handbook", "https://docs.example.com/", pattern = "\\.html$"))
#' build_store(src, "handbook.duckdb")   # once
#' refresh_store(src, "handbook.duckdb") # later, cheaply
#' }
refresh_store <- function(sources, path) {
  if (!file.exists(path)) {
    stop("no store at ", path, " -- build it first with `build_store()`")
  }
  check_ingest_deps()
  store <- tryCatch(
    ragnar::ragnar_store_connect(path, read_only = FALSE),
    error = function(e) {
      stop(sprintf(
        "could not open the store at '%s' for refresh: %s\nIf it is corrupt or from an incompatible ragnar version, rebuild it with build_store(overwrite = TRUE).",
        path, conditionMessage(e)), call. = FALSE)
    }
  )
  all_ok <- ingest_all(store, sources)
  ragnar::ragnar_store_build_index(store)
  invisible(all_ok)
}
