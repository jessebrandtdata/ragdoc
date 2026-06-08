#!/usr/bin/env Rscript
# Example MCP server for a ragdoc store.
#
# Build the store first (see `?build_store`), then point your coding agent at
# this script. Adjust the path, tool name, and description for your corpus.

suppressMessages(library(ragdoc))

serve_mcp(
  "handbook.duckdb",
  name        = "search_handbook",
  description = "Search the Example Co engineering handbook."
)
