# serve_mcp() assembles an MCP search tool over the store. The actual server
# (mcptools::mcp_server) blocks on a transport, so we mock it -- and ellmer's
# type/tool constructors -- to capture the assembled tool and inspect the pure
# wiring (the not-built fallback, the source-name hint) offline. Nothing here
# starts a server or reaches the network.

test_that("serve_mcp serves a 'not built' tool for a missing store path", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("mcptools")
  seen <- NULL
  local_mocked_bindings(
    tool = function(f, description, arguments, name) {
      list(f = f, name = name, arguments = arguments)
    },
    .package = "ellmer"
  )
  local_mocked_bindings(
    mcp_server = function(tools, ...) { seen <<- tools; invisible(NULL) },
    .package = "mcptools"
  )

  serve_mcp(file.path(tempdir(), "does-not-exist-xyz.duckdb"), name = "search_x")

  expect_length(seen, 1)
  expect_identical(seen[[1]]$name, "search_x")
  # The tool does not crash on an unbuilt store; it reports the state instead.
  expect_match(seen[[1]]$f("some query"), "not built")
})

test_that("serve_mcp surfaces the provided source names in the tool's source hint", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("mcptools")
  args_seen <- NULL
  local_mocked_bindings(
    # Return the description verbatim so we can assert on the assembled hint text.
    type_string = function(description, ...) description,
    type_integer = function(description, ...) description,
    tool = function(f, description, arguments, name) { args_seen <<- arguments; list() },
    .package = "ellmer"
  )
  local_mocked_bindings(
    mcp_server = function(tools, ...) invisible(NULL),
    .package = "mcptools"
  )

  serve_mcp(file.path(tempdir(), "no-store.duckdb"), sources = c("handbook", "api"))

  expect_match(args_seen$source, "handbook, api")
})

test_that("serve_mcp leaves the source hint empty when no sources are given", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("mcptools")
  args_seen <- NULL
  local_mocked_bindings(
    type_string = function(description, ...) description,
    type_integer = function(description, ...) description,
    tool = function(f, description, arguments, name) { args_seen <<- arguments; list() },
    .package = "ellmer"
  )
  local_mocked_bindings(
    mcp_server = function(tools, ...) invisible(NULL),
    .package = "mcptools"
  )

  serve_mcp(file.path(tempdir(), "no-store.duckdb"))

  expect_no_match(args_seen$source, "One of:")
})
