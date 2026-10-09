#' DAG layout
#'
#' How the Workflow extension arranges the board: the layered DAG layout, its
#' flow direction and its spacing. Users change both from the toolbar's
#' layout menu; [new_dag_extension()] takes the one a board starts with.
#'
#' @param direction `"TB"` (top to bottom) or `"LR"` (left to right). A wide
#'   board turned sideways grows in height instead.
#' @param spacing `"compact"`, `"normal"` or `"loose"`: the gaps between
#'   blocks and between layers.
#'
#' @return `dag_layout()` returns an object of class `dag_layout`: a list of
#'   `direction` and `spacing`, plain values only, so it round-trips through
#'   serialization.
#'
#' @examples
#' dag_layout()
#' dag_layout(direction = "LR", spacing = "compact")
#'
#' @rdname dag_layout
#' @export
dag_layout <- function(direction = c("TB", "LR"),
                       spacing = c("normal", "compact", "loose")) {
  structure(
    list(direction = match.arg(direction), spacing = match.arg(spacing)),
    class = "dag_layout"
  )
}

#' @rdname dag_layout
#' @param x Object to test.
#' @export
is_dag_layout <- function(x) inherits(x, "dag_layout")

# A layout as it comes back from serialization (a plain list), or `NULL`
# for the default.
as_dag_layout <- function(x) {

  if (is.null(x)) {
    return(dag_layout())
  }

  if (is_dag_layout(x)) {
    return(x)
  }

  if (is.list(x) && all(names(x) %in% c("direction", "spacing"))) {
    res <- tryCatch(
      dag_layout(
        direction = x[["direction"]] %||% "TB",
        spacing = x[["spacing"]] %||% "normal"
      ),
      error = function(e) NULL
    )
    if (!is.null(res)) {
      return(res)
    }
  }

  blockr_abort(
    "Expecting a layout from `dag_layout()`.",
    class = "dag_layout_invalid"
  )
}

# The gaps per spacing: between blocks in a layer, and between layers.
dag_layout_spacing <- function() {
  list(
    compact = list(nodesep = 20, ranksep = 30),
    normal = list(nodesep = 50, ranksep = 50),
    loose = list(nodesep = 80, ranksep = 90)
  )
}

# The g6R layout for a `dag_layout`, as `g6_layout()` takes it.
dag_layout_config <- function(x) {
  x <- as_dag_layout(x)
  c(
    list(
      type = "antv-dagre",
      rankdir = x$direction,
      begin = c(150, 150),
      sortByCombo = TRUE
    ),
    dag_layout_spacing()[[x$spacing]]
  )
}

# What the toolbar's layout menu needs to switch in the browser: the g6R
# layout per direction and spacing, as `dag_layout_config()` builds it.
dag_layout_catalog <- function() {
  lapply(
    set_names(nm = c("TB", "LR")),
    function(dir) {
      lapply(
        set_names(nm = names(dag_layout_spacing())),
        function(sp) dag_layout_config(dag_layout(dir, sp))
      )
    }
  )
}
