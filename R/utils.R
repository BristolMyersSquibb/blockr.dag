graph_id <- function(ns = NULL) {
  res <- "graph"

  if (is.null(ns)) {
    return(res)
  }

  ns(res)
}

last <- function(x) x[[length(x)]]

filter_null <- function(x) Filter(Negate(is.null), x)

has_length <- function(x) length(x) > 0L
