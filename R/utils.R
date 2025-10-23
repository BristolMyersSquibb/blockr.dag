graph_id <- function(ns = NULL) {

	res <- "graph"

  if (is.null(ns)) {
    return(res)
  }

  ns(res)
}
