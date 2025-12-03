#' Context menu functions
#'
#' Functions for creating and working with context
#' menu entries.
#'
#' @param name Name of the context menu entry.
#' @param js JavaScript code to execute when the entry is selected.
#' @param action Action to perform when the entry is selected.
#' @param condition Condition to determine if the entry should be shown.
#' @param id Unique identifier for the context menu entry.
#'   Inferred from `name` if not provided
#' @param x Object to test or extract context menu items from.
#'
#' @details
#' \describe{
#'   \item{`new_context_menu_entry()`}{Creates a new context menu
#' entry with the specified name, JavaScript code, action function,
#' and display condition.}
#'   \item{`is_context_menu_entry()`}{
#' Tests whether an object is a valid context menu entry.}
#'   \item{`context_menu_items()`}{Generic function to
#' extract context menu items from various
#' objects like dock extensions, boards, or lists.}
#' }
#'
#' The `context_menu_items.dag_extension()` method
#' provides the following actions:
#' \itemize{
#'   \item Create link - Creates connections between workflow nodes.
#'   \item Remove block - Removes individual blocks from the workflow.
#'   \item Remove link - Removes connections between workflow nodes.
#'   \item Append block - Adds a new block after the selected node.
#'   \item Create stack - Creates a new workflow stack.
#'   \item Remove stack - Removes an entire workflow stack.
#'   \item Edit stack - Opens stack editing interface.
#'   \item Add block - Adds a new block to the canvas.
#' }
#'
#' @rdname ctx
#' @export
#' @return
#' \describe{
#'   \item{`new_context_menu_entry()`}{A context menu
#' entry object of class "context_menu_entry" containing
#' condition, action, and js functions, with name and id attributes.}
#'   \item{`is_context_menu_entry()`}{`TRUE` if `x` is
#' a context menu entry, `FALSE` otherwise.}
#'   \item{`context_menu_items()`}{A list of context
#' menu items for the given object.}
#' }
new_context_menu_entry <- function(
  name,
  js,
  action = NULL,
  condition = TRUE,
  id = tolower(gsub(" +", "_", name))
) {
  if (is.null(action)) {
    action <- function(...) NULL
  }

  if (isTRUE(condition)) {
    condition <- function(...) TRUE
  }

  if (is_string(js)) {
    js_string <- js
    js <- function(...) js_string
  }

  stopifnot(
    is.function(action),
    is.function(condition),
    is.function(js),
    is_string(id),
    is_string(name)
  )

  structure(
    list(condition = condition, action = action, js = js),
    name = name,
    id = id,
    class = "context_menu_entry"
  )
}

#' @rdname ctx
#' @export
is_context_menu_entry <- function(x) {
  inherits(x, "context_menu_entry")
}

context_menu_entry_id <- function(x) attr(x, "id")

context_menu_entry_name <- function(x) attr(x, "name")

context_menu_entry_condition <- function(x, ...) {
  x[["condition"]](...)
}

context_menu_entry_action <- function(
  x,
  board,
  update,
  ...,
  domain = get_session()
) {
  if (!is_context_menu_entry(x)) {
    validate_context_menu_entries(x)

    for (i in x) {
      context_menu_entry_action(i, board, update, ..., domain = domain)
    }

    return(invisible(NULL))
  }

  fun <- x[["action"]]

  if (is.null(fun)) {
    return(invisible(NULL))
  }

  id <- context_menu_entry_id(x)

  res <- moduleServer(
    paste0("ctx_", id),
    fun(board, update, ..., domain = domain),
    domain
  )

  if (not_null(res)) {
    blockr_abort(
      "Expecting context menu item server {id} to return `NULL`.",
      class = "context_menu_item_return_invalid"
    )
  }

  invisible(NULL)
}

context_menu_entry_js <- function(x, ns = NULL) {
  if (!is_context_menu_entry(x)) {
    validate_context_menu_entries(x)

    res <- paste(
      chr_ply(x, context_menu_entry_js, ns = ns),
      collapse = " else "
    )

    return(
      paste0("(value, target, current) => {\n", res, "\n}")
    )
  }

  if (is.null(ns)) {
    ns <- NS(NULL)
  } else {
    ns <- NS(ns(paste0("ctx_", context_menu_entry_id(x))))
  }

  paste0(
    "if (value === '",
    context_menu_entry_id(x),
    "') {\n(",
    x[["js"]](ns),
    ")(value, target, current)\n}"
  )
}

build_context_menu <- function(x, ...) {
  if (!is_context_menu_entry(x)) {
    validate_context_menu_entries(x)

    res <- Filter(not_null, lapply(x, build_context_menu, ...))

    return(unname(res))
  }

  if (!context_menu_entry_condition(x, ...)) {
    return(NULL)
  }

  list(name = context_menu_entry_name(x), value = context_menu_entry_id(x))
}

validate_context_menu_entries <- function(x) {
  stopifnot(
    is.list(x),
    all(lgl_ply(x, is_context_menu_entry)),
    anyDuplicated(chr_ply(x, context_menu_entry_id)) == 0L
  )

  invisible(x)
}

#' @param x Object
#' @rdname ctx
#' @export
context_menu_items <- function(x) {
  UseMethod("context_menu_items")
}

#' @export
context_menu_items.dock_extension <- function(x) {
  list()
}

#' @export
context_menu_items.list <- function(x) {
  res <- lapply(x, context_menu_items)
  unlst(res)
}

#' @export
context_menu_items.dock_extensions <- function(x) {
  context_menu_items(as.list(x))
}

#' @export
context_menu_items.dock_board <- function(x) {
  context_menu_items(blockr.dock::dock_extensions(x))
}
