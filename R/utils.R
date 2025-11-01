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
        package = attr(info, "package"),
        icon = attr(info, "icon")
      )

      return(res)
    }
  }

  list(
    category = "Uncategorized",
    name = block_name(x),
    description = "No description available",
    package = "local",
    icon = "question-square"
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

#' Create SVG data URI for node icon
#'
#' Creates an SVG with a colored circle background and white icon,
#' then converts it to a data URI for use in g6 image nodes.
#'
#' @param icon_name Bootstrap icon name
#' @param color Hex color for background
#' @param size Size of the SVG in pixels (default: 48)
#' @return Character string with data URI
#' @keywords internal
blk_icon_data_uri <- function(icon_name, color, size = 48) {

  if (!pkg_avail("bsicons")) {
    return("")
  }

  # Get the icon SVG
  icon_svg <- as.character(bsicons::bs_icon(icon_name))

  # Extract the path element - bsicons returns <path ...></path>
  # Match from <path to </path>
  path_match <- regexpr('<path[^>]*>.*?</path>', icon_svg, perl = TRUE)
  if (path_match == -1) {
    return("")
  }
  icon_path <- regmatches(icon_svg, path_match)

  # Create a complete SVG with colored rounded square background and centered icon
  # Icon is scaled to about 60% of the square size
  icon_size <- size * 0.6
  icon_offset <- (size - icon_size) / 2

  # Rounded corner radius (about 15% of size for nice rounded corners)
  corner_radius <- size * 0.15

  svg <- sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">
      <rect x="0" y="0" width="%d" height="%d" rx="%f" ry="%f" fill="%s"/>
      <g transform="translate(%f, %f) scale(%f)" fill="white">
        %s
      </g>
    </svg>',
    size, size, size, size,
    size, size, corner_radius, corner_radius, color,
    icon_offset, icon_offset, icon_size / 16,  # bsicons are 16x16 by default
    icon_path
  )

  # Convert to base64 data URI
  svg_base64 <- jsonlite::base64_enc(charToRaw(svg))
  sprintf("data:image/svg+xml;base64,%s", svg_base64)
}

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
