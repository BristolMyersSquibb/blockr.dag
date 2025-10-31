#' DAG stack
#'
#' Extends [blockr.core::new_stack()] by adding a color attribute.
#'
#' @param ... Forwarded to [blockr.core::new_stack()]
#' @param color String-valued color
#'
#' @rdname stack
#' @export
new_dag_stack <- function(..., color = suggest_new_colors()) {
  new_stack(..., color = color, class = "dag_stack")
}

#' @param x object
#' @rdname stack
#' @export
is_dag_stack <- function(x) {
  inherits(x, "dag_stack")
}

#' @export
validate_stack.dag_stack <- function(x) {

  col <- stack_color(NextMethod())

  if (!is_string(col) || !is_hex_color(col)) {
    blockr_abort(
      "Expecting stack color as string-valued hex color.",
      class = "invalid_stack_color"
    )
  }

  x
}

is_hex_color <- function(x) {
  grepl("^#(?:[0-9a-fA-F]{3}){1,2}$", x)
}

#' @rdname stack
#' @export
stack_color <- function(x) {

  if (is_dag_stack(x)) {
    return(attr(x, "color"))
  }

  if (is_stack(x)) {
    return(NULL)
  }

  blockr_abort(
    "Cannot return stack color for objects with class{?es} {class(x)}",
    class = "stack_color_not_available"
  )
}

#' @param value Replacement value
#' @rdname stack
#' @export
`stack_color<-` <- function(x, value) {
  stopifnot(is_dag_stack(x), is_string(value), is_hex_color(value))
  attr(x, "color") <- value
  x
}

#' @rdname stack
#' @export
as_dag_stack <- function(x, ...) {
  UseMethod("as_dag_stack")
}

#' @export
as_dag_stack.dag_stack <- function(x, ...) {
  x
}

#' @export
as_dag_stack.stack <- function(x, color = suggest_new_colors(), ...) {

  attr(x, "color") <- color

  class(x) <- c("dag_stack", class(x))

  validate_stack(x)
}

#' @export
as_dag_stack.default <- function(x, ...) {
  as_dag_stack(as_stack(x), ...)
}

#' @export
format.dag_stack <- function(x, ...) {
  res <- NextMethod()
  c(res[c(1L, 2L)], paste0("Color: \"", stack_color(x), "\""), res[-c(1L, 2L)])
}
