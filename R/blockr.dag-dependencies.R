#' blockr.dag dependencies utils
#'
#' @description This function attaches blockr.dag dependencies to the given tag
#'
#' @param tag Element to attach the dependencies.
#'
#' @importFrom utils packageVersion
#' @importFrom htmltools tagList htmlDependency
#' @export
add_blockr.dag_deps <- function(tag) {
  blockr.dag_deps <- htmlDependency(
    name = "blockr.dag",
    version = "0.0.1",
    src = c(file = "blockr.dag-0.0.1"),
    script = "dist/blockr.dag.min.js",
    stylesheet = "dist/blockr.dag.min.css",
    package = "blockr.dag",
  )
  tagList(tag, blockr.dag_deps)
}
