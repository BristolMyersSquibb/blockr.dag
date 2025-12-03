dag_ext_ui <- function(id, board) {
  ns <- NS(id)
  has_blocks <- length(board_blocks(board)) > 0

  # nolint start: line_length_linter
  icon_pointer <- paste0(
    '<svg width="20" height="20" viewBox="0 0 24 24" ',
    'fill="none" stroke="#9ca3af" stroke-width="2">',
    '<path d="M3 3l7.07 16.97 2.51-7.39 7.39-2.51L3 3z"/></svg>'
  )
  icon_plus <- paste0(
    '<svg width="16" height="16" viewBox="0 0 24 24" ',
    'fill="none" stroke="#d1d5db" stroke-width="2">',
    '<line x1="12" y1="5" x2="12" y2="19"/>',
    '<line x1="5" y1="12" x2="19" y2="12"/></svg>'
  )
  icon_workflow <- paste0(
    '<svg width="20" height="20" viewBox="0 0 24 24" ',
    'fill="none" stroke="#9ca3af" stroke-width="2">',
    '<rect x="3" y="3" width="6" height="6" rx="1"/>',
    '<rect x="15" y="3" width="6" height="6" rx="1"/>',
    '<rect x="9" y="15" width="6" height="6" rx="1"/>',
    '<path d="M6 9v3a3 3 0 0 0 3 3h6a3 3 0 0 0 3-3V9"/></svg>'
  )
  # nolint end

  tagList(
    tags$div(
      class = "dag-canvas-container",
      style = "position: relative; width: 100%; height: 100vh;",
      g6_output(graph_id(ns), height = "100vh"),
      tags$div(
        id = ns("empty-state"),
        class = "dag-empty-state",
        style = if (has_blocks) "display: none;" else NULL,
        tags$div(
          class = "dag-empty-state-content",
          tags$div(
            class = "dag-empty-state-icons",
            tags$div(class = "dag-empty-state-icon-box", HTML(icon_pointer)),
            HTML(icon_plus),
            tags$div(class = "dag-empty-state-icon-box", HTML(icon_workflow))
          ),
          tags$p(
            class = "dag-empty-state-title",
            "Start building your workflow"
          ),
          tags$div(
            class = "dag-empty-state-hints",
            tags$p(
              class = "dag-empty-state-hint",
              tags$kbd("Right-click"),
              " to add blocks"
            ),
            tags$p(
              class = "dag-empty-state-hint",
              tags$kbd("Shift"),
              " + ",
              tags$kbd("Drag"),
              " to connect"
            )
          )
        )
      )
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
      stylesheet = file.path("css", "empty-state.css"),
      package = pkg_name()
    )
  )
}
