dag_ext_ui <- function(id, board) {
  ns <- shiny::NS(id)
  tagList(
    g6_output(graph_id(ns), height = "100%")
  )
}
