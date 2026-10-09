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
#' @param retarget Deprecated and ignored. It made an entry follow the
#'   selection while it held a pinned sidebar panel; the dock's actions open
#'   menus now, which close on the next click, so there is nothing to follow.
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
#'   \item Connect to - Links the block to another one, either way.
#'   \item Append block - Adds a new block after the node.
#'   \item Add to stack - Puts the block, or the selection it is part of,
#'     into a stack or a new one.
#'   \item Remove block - Removes the block.
#'   \item Edit link - Opens the link's menu.
#'   \item Insert block - Puts a new block into the link.
#'   \item Remove link - Removes the link.
#'   \item Edit stack - Opens the stack's menu.
#'   \item Dissolve stack - Removes the stack; its blocks stay.
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
  id = tolower(gsub(" +", "_", name)),
  retarget = FALSE
) {
  if (isTRUE(retarget)) {
    blockr_warn(
      "`retarget` is deprecated and ignored: the dock's actions open menus, ",
      "which close on the next click, so there is no panel to follow.",
      class = "context_menu_entry_retarget_deprecated",
      frequency = "once",
      frequency_id = "context_menu_entry_retarget"
    )
  }

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

context_menu_entry_action <- function(x, actions, session = get_session()) {

  if (!is_context_menu_entry(x)) {

    res <- lapply(
      validate_context_menu_entries(x),
      context_menu_entry_action,
      actions,
      session
    )

    return(invisible(res))
  }

  stopifnot(is_context_menu_entry(x))

  fun <- x[["action"]]

  if (is.null(fun)) {
    return(invisible(NULL))
  }

  res <- fun(actions, session)

  if (!inherits(res, "Observer")) {
    blockr_abort(
      "Expecting context menu item server {context_menu_entry_id(x)} to ",
      "return an observer.",
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

    # A destructive row comes last, in a group of its own (dag.css draws the
    # divider above it).
    rm <- grepl("^remove_", chr_xtr(res, "value"))

    return(unname(c(res[!rm], res[rm])))
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
