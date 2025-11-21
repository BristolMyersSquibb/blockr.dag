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

  if (is_string(js)) {
    js_string <- js
    js <- function(...) js_string
  }

  stopifnot(
    is_string(id),
    is_string(icon),
    is.null(action) || is.function(action),
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

toolbar_item_action <- function(x, board, update, session = get_session()) {

  if (!is_toolbar_item(x)) {

    validate_toolbar_items(x)

    for (i in x) {
      toolbar_item_action(i, board, update)
    }

    return(invisible(NULL))
  }

  fun <- x[["action"]]

  if (is.null(fun)) {
    return(invisible(NULL))
  }

  id <- toolbar_item_id(x)

  res <- moduleServer(
    paste0("tool_", id),
    fun(board, update),
    session
  )

  if (not_null(res)) {
    blockr_abort(
      "Expecting toolbar item server {id} to return `NULL`.",
      class = "toolbar_item_return_invalid"
    )
  }

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
  } else {
    ns <- NS(ns(paste0("tool_", toolbar_item_id(x))))
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

#' @rdname tool
#' @export
toolbar_items <- function(x) {
  UseMethod("toolbar_items")
}

#' @export
toolbar_items.dock_extension <- function(x) {
  list()
}

#' @export
toolbar_items.list <- function(x) {

  res <- lapply(x, toolbar_items)

  for (x in res) {
    validate_toolbar_items(x)
  }

  coal(unlst(res), list())
}

#' @export
toolbar_items.dock_extensions <- function(x) {
  res <- toolbar_items(as.list(x))
  validate_toolbar_items(res)
  res
}

#' @export
toolbar_items.dock_board <- function(x) {
  toolbar_items(blockr.dock::dock_extensions(x))
}
