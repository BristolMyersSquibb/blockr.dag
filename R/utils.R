graph_id <- function(ns = NULL) {
  res <- "graph"

  if (is.null(ns)) {
    return(res)
  }

  ns(res)
}

#' Get block info in registry
#'
#' @param x Block object
#' @keywords internal
get_block_metadata <- function(x) {
  stopifnot(is_block(x))

  ctor <- attr(x, "ctor")
  ctor <- attr(ctor, "fun")

  if (is_string(ctor)) {
    blk <- sub("^new_", "", ctor)
    blks <- available_blocks()

    if (blk %in% names(blks)) {
      info <- blks[[blk]]

      res <- list(
        category = attr(info, "category"),
        name = attr(info, "name"),
        description = attr(info, "description"),
        package = attr(info, "package")
      )

      return(res)
    }
  }

  list(
    category = "Uncategorized",
    name = block_name(x),
    description = "No description available",
    package = "local"
  )
}

#' @keywords internal
blk_color <- function(category) {
  # Palette is taken from:
  # https://siegal.bio.nyu.edu/color-palette/
  # very nice palette that is color-blind friendly.
  switch(
    category,
    data = "#0072B2",
    transform = "#56B4E9",
    plot = "#E69F00",
    file = "#CC79A7",
    parse = "#009E73",
    table = "#F0E442",
    text = "#D55E00",
    "#6c757d"
  )
}

#' Get block icon name
#'
#' Returns the FontAwesome icon name for a block category.
#' This is aligned with blockr.ui::blk_icon() but returns just the name.
#'
#' @param category Block category
#' @keywords internal
blk_icon_name <- function(category) {
  if (!length(category)) {
    return("cube")
  }

  switch(
    category,
    data = "table",
    file = "file",
    parse = "gear",
    plot = "chart-line",
    transform = "wand-magic",
    table = "table",
    "cube"
  )
}

last <- function(x) x[[length(x)]]
