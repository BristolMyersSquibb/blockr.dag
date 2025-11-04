graph_id <- function(ns = NULL) {
  res <- "graph"

  if (is.null(ns)) {
    return(res)
  }

  ns(res)
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
  # Okabe-Ito colorblind-friendly palette
  # See: https://jfly.uni-koeln.de/color/
  switch(
    category,
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

filter_null <- function(x) Filter(Negate(is.null), x)

#' Create icon data URI for g6 image nodes
#'
#' Generates an SVG data URI with a colored rounded square background and white
#' icon for use in g6 DAG visualization.
#'
#' @param icon_svg Character string containing the SVG icon markup
#' @param color Hex color code for the background
#' @param size Numeric size in pixels (default: 48)
#' @return Character string containing a data URI
#' @keywords internal
blk_icon_data_uri <- function(icon_svg, color, size = 48) {

  stopifnot(is_string(icon_svg), is_string(color), is.numeric(size))

  # Extract the path/content from the icon SVG
  # Icon SVG is typically: <svg ...><path d="..."/></svg>
  # We want just the inner content
  icon_content <- sub("^<svg[^>]*>", "", icon_svg)
  icon_content <- sub("</svg>$", "", icon_content)

  # Create outer SVG with colored rounded rectangle and white icon
  icon_size <- size * 0.6  # Icon takes 60% of total size
  icon_offset <- size * 0.2  # Center the icon
  corner_radius <- size * 0.15  # 15% corner radius

  svg <- sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">
  <rect width="%d" height="%d" rx="%g" ry="%g" fill="%s"/>
  <g transform="translate(%g, %g)" fill="white">
    <svg width="%g" height="%g" viewBox="0 0 16 16">%s</svg>
  </g>
</svg>',
    size, size, size, size,
    size, size, corner_radius, corner_radius, color,
    icon_offset, icon_offset,
    icon_size, icon_size, icon_content
  )

  # Convert to base64 data URI
  sprintf(
    "data:image/svg+xml;base64,%s",
    jsonlite::base64_enc(charToRaw(svg))
  )
}
