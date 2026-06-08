#' Serve a store to a coding agent over MCP
#'
#' Wraps a documentation store in a single Model Context Protocol search tool
#' and starts an MCP server exposing it. Point your agent at the script that
#' calls this and it gains one tool, `name`, that runs [search_docs()] and
#' returns cited passages.
#'
#' `store` may be a connected store (from [connect_store()]) or a path to one.
#' Given a path that does not exist yet, the server still starts and the tool
#' reports that the store is not built rather than crashing -- handy while you
#' are still wiring things up.
#'
#' Requires the suggested packages \pkg{ellmer} and \pkg{mcptools}.
#'
#' @param store A connected ragnar store (see [connect_store()]) or a path to a
#'   store created by [build_store()].
#' @param name Name of the MCP tool the agent will see. Defaults to
#'   `"search_docs"`.
#' @param description Tool description shown to the agent. Make it specific to
#'   your corpus so the agent knows when to reach for it.
#' @param sources Optional character vector of source names, surfaced in the
#'   tool's `source` argument description to hint at valid filter values.
#'
#' @return Called for its side effect of running the MCP server; does not return.
#' @seealso [connect_store()], [search_docs()]
#' @export
#' @examples
#' \dontrun{
#' # Pass the store path -- the server starts even before the store is built.
#' serve_mcp("handbook.duckdb",
#'   name        = "search_handbook",
#'   description = "Search the Example Co engineering handbook.")
#' }
serve_mcp <- function(store,
                      name = "search_docs",
                      description = "Search the documentation store.",
                      sources = NULL) {
  for (pkg in c("ellmer", "mcptools")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("serve_mcp() needs the '%s' package; install it first", pkg))
    }
  }

  src_hint <- if (length(sources)) {
    sprintf(" One of: %s.", paste(sources, collapse = ", "))
  } else {
    ""
  }
  arguments <- list(
    query  = ellmer::type_string("Natural-language search query", required = TRUE),
    n      = ellmer::type_integer("Number of passages to return (default 8)", required = FALSE),
    source = ellmer::type_string(
      paste0("Restrict to one source by name.", src_hint), required = FALSE)
  )

  if (is.character(store)) {
    if (!file.exists(store)) {
      not_built <- ellmer::tool(
        function(query, n = 8, source = NULL) "store not built yet -- run your build script",
        description = description, arguments = arguments, name = name
      )
      return(mcptools::mcp_server(tools = list(not_built)))
    }
    store <- connect_store(store)
  }

  tool <- ellmer::tool(
    function(query, n = 8, source = NULL) {
      if (is.null(n)) n <- 8
      search_docs(store, query, n = n, source = source)
    },
    description = description, arguments = arguments, name = name
  )
  mcptools::mcp_server(tools = list(tool))
}
