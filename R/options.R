new_stack_colors_option <- function(
  n_stacks = blockr_option("n_stacks", 40L),
  color_palette = blockr_option("stacks_palette", "Spectral"),
  ...
) {
  new_board_option(
    id = "stack_colors",
    default = list(n_stacks = n_stacks, color_palette = color_palette),
    ui = function(id) {
      span(
        numericInput(
          NS(id, "n_stacks"),
          "Number of stack colors",
          n_stacks,
          min = 1L,
          step = 1L
        ),
        selectInput(
          NS(id, "color_palette"),
          "Color palette",
          grDevices::hcl.pals(),
          color_palette
        )
      )
    },
    server = function(..., session) {
      observeEvent(
        get_board_option_or_null("stack_colors", session),
        {
          opt <- get_board_option_value("stack_colors", session)
          updateNumericInput(
            session,
            "n_stacks",
            value = attr(opt, "n_stacks")
          )
          updateSelectInput(
            session,
            "color_palette",
            selected = attr(opt, "palette")
          )
        }
      )
    },
    update_trigger = c("n_stacks", "color_palette"),
    transform = function(x) {
      structure(
        grDevices::hcl.colors(x$n_stacks, palette = x$color_palette),
        n_stacks = as.integer(x$n_stacks),
        palette = x$color_palette
      )
    },
    ...
  )
}

#' @export
validate_board_option.stack_colors_option <- function(x) {
  val <- board_option_value(NextMethod())

  nst <- attr(val, "n_stacks")
  pal <- attr(val, "palette")

  if (!is_count(nst)) {
    blockr_abort(
      "Expecting `n_stacks` to represent a count.",
      class = "board_options_stack_colors_invalid"
    )
  }

  if (!(is_string(pal) && pal %in% grDevices::hcl.pals())) {
    blockr_abort(
      "Expecting `color_palette` to represent a single valid color palette.",
      class = "board_options_stack_colors_invalid"
    )
  }

  if (!(is.character(val) && length(val) == nst)) {
    blockr_abort(
      paste0(
        "Expecting `stack_colors` to be a character vector of length ",
        nst,
        "."
      ),
      class = "board_options_stack_colors_invalid"
    )
  }

  invisible(x)
}

#' @export
blockr_ser.stack_colors_option <- function(x, option = NULL, ...) {
  val <- coal(option, board_option_value(x))

  NextMethod(
    option = list(
      n_stacks = attr(val, "n_stacks"),
      color_palette = attr(val, "palette")
    )
  )
}
