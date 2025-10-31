dag_ext_ui <- function(id, board) {
  ns <- shiny::NS(id)
  add_blockr.dag_deps(
    g6_output(graph_id(ns), height = "100vh")
  )
}
