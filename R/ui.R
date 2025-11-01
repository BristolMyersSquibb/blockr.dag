dag_ext_ui <- function(id, board) {
  ns <- shiny::NS(id)
  tagList(
    context_menu_dep(),
    g6_output(graph_id(ns), height = "100vh")
  )
}

context_menu_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-dag-context-menu",
    version = utils::packageVersion("blockr.dag"),
    src = system.file("assets", package = "blockr.dag"),
    script = "js/context-menu.js",
    all_files = FALSE
  )
}
