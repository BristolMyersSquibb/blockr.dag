dag_ext_ui <- function(id, board) {
  tagList(
    g6_output(graph_id(NS(id)), height = "100vh"),
    htmltools::htmlDependency(
      name = "rm-selection",
      version = pkg_version(),
      src = c(file = "assets"),
      script = file.path("js", "rm-sel.js"),
      package = pkg_name()
    )
  )
}
