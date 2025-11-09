graph_id <- function(ns = NULL) {
  res <- "graph"

  if (is.null(ns)) {
    return(res)
  }

  ns(res)
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

has_length <- function(x) length(x) > 0L
