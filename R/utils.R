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
    parse = "input",
    text = "utility",
    aiml = "model",  # AI/ML merged into model
    timeseries = "structured",  # Old name for structured
    # New categories (pass through)
    input = "input",
    transform = "transform",
    structured = "structured",
    plot = "plot",
    table = "table",
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
    transform = "#009E73", # Bluish green
    structured = "#56B4E9", # Sky blue
    plot = "#E69F00", # Orange
    table = "#CC79A7", # Reddish purple/pink
    model = "#F0E442", # Yellow (includes AI/ML)
    output = "#D55E00", # Vermilion
    utility = "#CCCCCC", # Light gray
    "#999999" # Medium gray (uncategorized)
  )
}

last <- function(x) x[[length(x)]]

suggest_new_colors <- function(colors = character(), n = 1) {

  color_fun <- blockr_option("stack_color", next_color)

  stopifnot(is.function(color_fun), is.character(colors), is_count(n))

  res <- character()

  for (i in seq_len(n)) {
    res <- c(res, color_fun(c(colors, res)))
  }

  res
}

next_color <- function(colors = character(), lum_var = TRUE) {

  if (!pkg_avail("colorspace")) {
    blockr_abort(
      "Package 'colorspace' is required.",
      class = "colorspace_not_available"
    )
  }

  if (length(colors)) {

    prev_hcl <- colorspace::coords(
      methods::as(colorspace::hex2RGB(colors), "polarLUV")
    )

    base_l <- mean(prev_hcl[, "L"], na.rm = TRUE)
    base_c <- mean(prev_hcl[, "C"], na.rm = TRUE)
    base_h <- mean(prev_hcl[, "H"], na.rm = TRUE)

  } else {

    base_l <- 65
    base_c <- 60
    base_h <- 0
  }

  # Golden angle in degrees
  golden_angle <- 137.508

  idx <- length(colors)

  # Compute hues via golden-angle rotation
  new_h <- (base_h + idx * golden_angle) %% 360

  # Optional gentle luminance modulation for visual distinction
  if (isTRUE(lum_var)) {
    new_l <- base_l + 10 * sin(idx * pi / 3)
  } else {
    new_l <- base_l
  }

  new_c <- base_c

  # Convert back to hex
  colorspace::hex(
    colorspace::polarLUV(L = new_l, C = new_c, H = new_h),
    fixup = TRUE
  )
}

available_stack_blocks <- function(board) {

  stacks <- board_stacks(board)
  blocks <- board_blocks(board)

  blk_ids <- names(blocks)

  stacked_blocks <- unlst(
    lapply(stacks, stack_blocks)
  )

  blk_ids[!(blk_ids %in% stacked_blocks)]
}
