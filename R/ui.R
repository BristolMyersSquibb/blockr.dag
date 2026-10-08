dag_ext_ui <- function(id, board) {
  ns <- NS(id)
  has_blocks <- length(board_blocks(board)) > 0

  tagList(
    tags$div(
      class = "dag-canvas-container",
      style = "position: relative; width: 100%; height: 100vh;",
      g6_output(graph_id(ns), height = "100vh"),
      # An empty panel says so in one italic line (design system, Messages).
      tags$div(
        id = ns("empty-state"),
        class = "dag-empty-state",
        style = if (has_blocks) "display: none;" else NULL,
        tags$p(
          class = "blockr-empty",
          "No blocks yet. Right-click to add one."
        )
      )
    ),
    blockr.ui::controls_dep(),
    htmltools::htmlDependency(
      name = "dag-chrome",
      version = pkg_version(),
      src = c(file = "assets"),
      script = file.path("js", "dag-chrome.js"),
      stylesheet = file.path("css", "dag.css"),
      package = pkg_name()
    ),
    htmltools::htmlDependency(
      name = "rm-selection",
      version = pkg_version(),
      src = c(file = "assets"),
      script = file.path("js", "rm-sel.js"),
      package = pkg_name()
    ),
    htmltools::htmlDependency(
      name = "dag-empty-state",
      version = pkg_version(),
      src = c(file = "assets"),
      script = file.path("js", "empty-state.js"),
      package = pkg_name()
    ),
    htmltools::htmlDependency(
      name = "copy-paste",
      version = pkg_version(),
      src = c(file = "assets"),
      script = file.path("js", "copy-paste.js"),
      package = pkg_name()
    )
  )
}
