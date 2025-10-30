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

#' Remap old category names to new category names
#'
#' Provides backward compatibility by mapping old category names to the new
#' standardized category system.
#'
#' @param category Character string with category name (old or new)
#' @return Character string with new category name
#' @keywords internal
remap_category <- function(category) {
  if (!length(category) || is.na(category)) {
    return("uncategorized")
  }

  # Old → New category mapping
  mapping <- c(
    # Old categories
    data = "input",
    file = "input",
    table = "output",
    parse = "input",
    text = "utility",
    # New categories (pass through)
    input = "input",
    transform = "transform",
    timeseries = "timeseries",
    plot = "plot",
    model = "model",
    output = "output",
    utility = "utility",
    uncategorized = "uncategorized"
  )

  # Get mapped category or return uncategorized if not found
  mapped <- mapping[category]
  if (is.na(mapped)) {
    return("uncategorized")
  }

  as.character(mapped)
}

#' Get block color
#'
#' Returns the color hex code for a block category using the Okabe-Ito
#' colorblind-friendly palette.
#'
#' @param category Block category (old or new format)
#' @return Character string with hex color code
#' @keywords internal
blk_color <- function(category) {
  # Remap old categories to new ones
  category <- remap_category(category)

  # Okabe-Ito colorblind-friendly palette
  # See: https://jfly.uni-koeln.de/color/
  switch(category,
    input = "#0072B2", # Blue
    transform = "#009E73", # Bluish green (swapped with timeseries)
    timeseries = "#56B4E9", # Sky blue (swapped with transform)
    plot = "#E69F00", # Orange
    model = "#F0E442", # Yellow
    output = "#D55E00", # Vermilion
    utility = "#CCCCCC", # Light gray
    "#999999" # Medium gray (uncategorized)
  )
}

#' Get block icon name
#'
#' Returns the FontAwesome icon name for a block category.
#'
#' @param category Block category (old or new format)
#' @return Character string with FontAwesome icon name
#' @keywords internal
blk_icon_name <- function(category) {
  if (!length(category)) {
    return("shapes")
  }

  # Remap old categories to new ones
  category <- remap_category(category)

  switch(category,
    input = "folder-open",
    transform = "shuffle",
    timeseries = "chart-line",
    plot = "chart-simple",
    model = "brain",
    output = "file-export",
    utility = "wrench",
    "shapes" # uncategorized
  )
}

last <- function(x) x[[length(x)]]
