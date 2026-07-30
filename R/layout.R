#' DAG layout
#'
#' Layout for the DAG view, used as the `layout` argument of
#' [new_dag_extension()]. blockr DAGs are directed, multi-parent graphs, so the
#' only sensible layouts are the layered (dagre) family; `dag_layout()` wraps
#' g6's `antv-dagre` with the handful of knobs that matter and validates them,
#' rather than exposing g6R's full (mostly unsuitable) layout surface.
#'
#' The default flows top-to-bottom (`rankdir = "TB"`), the natural reading for
#' the common small board. On large boards a top-down layout collides the
#' sibling labels (blockr nodes label below the icon) and sprawls horizontally;
#' switching to `rankdir = "LR"` there spreads siblings vertically and reserves
#' label width along the rank axis, removing overlap and keeping the graph much
#' narrower.
#'
#' @param rankdir Flow direction: `"TB"` (default), `"LR"`, `"BT"` or `"RL"`.
#' The DAG's edge curve and port sides are derived from this.
#' @param nodesep Separation between nodes in the same rank, in px.
#' @param ranksep Separation between ranks, in px.
#' @param ranker Rank-assignment algorithm: `"network-simplex"` (default),
#' `"tight-tree"` or `"longest-path"`.
#' @param node_size Collision box `c(width, height)` (or a single number) used
#' for spacing. Set the width to roughly the widest label so labels don't
#' overlap. `NULL` (default) leaves dagre's own spacing for top-down flow and
#' reserves label width for the left-to-right flows where it's needed.
#' @param sort_by_combo Keep stacked (combo) nodes together during layout.
#'
#' @return A g6 `antv-dagre` layout (a plain list), suitable for
#' [new_dag_extension()] and JSON-serialisable for save / restore.
#' @rdname dag_layout
#' @export
dag_layout <- function(rankdir = c("TB", "LR", "BT", "RL"),
                       nodesep = 50,
                       ranksep = 50,
                       ranker = c("network-simplex", "tight-tree",
                                  "longest-path"),
                       node_size = NULL,
                       sort_by_combo = TRUE) {
  rankdir <- match.arg(rankdir)
  ranker <- match.arg(ranker)

  if (is.null(node_size) && rankdir %in% c("LR", "RL")) {
    # In horizontal flow the label extends along the rank axis, so reserve its
    # width to keep neighbouring ranks' labels apart.
    node_size <- c(140, 40)
  }

  stopifnot(
    is.numeric(nodesep), length(nodesep) == 1L,
    is.numeric(ranksep), length(ranksep) == 1L,
    is.null(node_size) ||
      (is.numeric(node_size) && length(node_size) %in% c(1L, 2L)),
    is.logical(sort_by_combo), length(sort_by_combo) == 1L
  )

  antv_dagre_layout(
    begin = c(150, 150),
    rankdir = rankdir,
    nodesep = nodesep,
    ranksep = ranksep,
    ranker = ranker,
    nodeSize = node_size,
    sortByCombo = sort_by_combo
  )
}

# Default DAG layout (top-to-bottom). Internal: `set_g6_layout()` falls back to
# it when no layout is supplied, and `state` echoes NULL (not this object) so
# the default can evolve without touching restored boards.
default_dag_layout <- function() {
  dag_layout()
}

# Flow direction of a (resolved) DAG layout, defaulting to "TB" so callers
# without a layout keep the historical orientation.
layout_rankdir <- function(layout) {
  coal(layout[["rankdir"]], "TB")
}

# Port sides and edge curve derived from the flow direction. `axis` / `base`
# describe where multi-input ports sit: along the x edge (`axis = "x"`) at
# y = `base`, or along the y edge at x = `base`.
dag_orientation <- function(rankdir = "TB") {
  switch(
    rankdir,
    TB = list(input = "top", output = "label-bottom",
              edge = "cubic-vertical", axis = "x", base = 0),
    BT = list(input = "bottom", output = "top",
              edge = "cubic-vertical", axis = "x", base = 1),
    LR = list(input = "left", output = "right",
              edge = "cubic-horizontal", axis = "y", base = 0),
    RL = list(input = "right", output = "left",
              edge = "cubic-horizontal", axis = "y", base = 1),
    list(input = "top", output = "label-bottom",
         edge = "cubic-vertical", axis = "x", base = 0)
  )
}

# Fractional placement for a multi-input port at position `frac` along the
# node's input edge.
port_placement <- function(orient, frac) {
  if (identical(orient[["axis"]], "x")) {
    c(frac, orient[["base"]])
  } else {
    c(orient[["base"]], frac)
  }
}
