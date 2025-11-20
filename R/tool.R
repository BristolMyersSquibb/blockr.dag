#' Create a toolbar item
#'
#' Utilities to create tooblbar items for working with the DAG.
#'
#' @param id Unique identifier for the toolbar item
#' @param icon Name of an icon to show in the toolbar
#' @param js JavaScript code to execute when the entry is selected
#' @param action Action to perform when the entry is selected
#'
#' @rdname tool
#' @export
new_toolbar_item <- function(id, icon, js, action = NULL) {

  if (is.null(action)) {
    action <- function(...) NULL
  }

  if (is_string(js)) {
    js_string <- js
    js <- function(...) js_string
  }

  stopifnot(
    is_string(id),
    is_string(icon),
    is.function(action),
    is.function(js)
  )

  structure(
    list(action = action, js = js),
    id = id,
    icon = icon,
    class = "toolbar_item"
  )
}

#' @param x Object
#' @rdname tool
#' @export
is_toolbar_item <- function(x) {
  inherits(x, "toolbar_item")
}

validate_toolbar_items <- function(x) {

  stopifnot(
    is.list(x),
    all(lgl_ply(x, is_toolbar_item)),
    anyDuplicated(chr_ply(x, toolbar_item_id)) == 0L
  )

  invisible(x)
}

toolbar_item_id <- function(x) attr(x, "id")

toolbar_item_icon <- function(x) attr(x, "icon")

toolbar_item_action <- function(x, ...) {

  if (!is_toolbar_item(x)) {

    validate_toolbar_items(x)

    for (i in x) {
      toolbar_item_action(i, ...)
    }

    return(invisible(NULL))
  }

  id <- toolbar_item_id(x)

  moduleServer(
    id,
    function(input, output, session) {
      x[["action"]](...)
    }
  )

  invisible(NULL)
}

toolbar_item_js <- function(x, ns = NULL) {

  if (!is_toolbar_item(x)) {

    validate_toolbar_items(x)

    res <- paste(
      chr_ply(x, toolbar_item_js, ns = ns),
      collapse = " else "
    )

    return(
      paste0("(value, target, current) => {\n", res, "\n}")
    )
  }

  if (is.null(ns)) {
    ns <- NS(NULL)
  }

  paste0(
    "if (value === '",
    toolbar_item_id(x),
    "') {\n(",
    x[["js"]](ns),
    ")(value, target, current)\n}"
  )
}

build_toolbar <- function(x, ...) {

  if (!is_toolbar_item(x)) {

    validate_toolbar_items(x)

    res <- paste(
      chr_ply(x, build_toolbar, ...),
      collapse = ",\n"
    )

    return(
      paste0("( ) => [\n", res, "\n]")
    )
  }

  sprintf(
    "{ id : '%s' , value : '%s' }",
    toolbar_item_icon(x),
    toolbar_item_id(x)
  )
}
