#' DAG stack
#'
#' Extends [blockr.core::new_stack()] by adding a color attribute.
#'
#' @param ... Forwarded to [blockr.core::new_stack()].
#' @param color String-valued color.
#'
#' @details
#' \describe{
#'   \item{`new_dag_stack()`}{Creates a new DAG
#' stack with color attribute.}
#'   \item{`is_dag_stack()`}{Tests if object is a DAG stack.}
#'   \item{`validate_stack.dag_stack()`}{Validates
#' DAG stack structure and color.}
#'   \item{`stack_color<-()`}{Sets the color
#' attribute of a DAG stack.}
#'   \item{`as_dag_stack()`}{Converts objects to DAG stack.}
#' }
#'
#' @return
#' \describe{
#'   \item{`new_dag_stack()`}{A `dag_stack` object}
#'   \item{`is_dag_stack()`}{Logical value
#' indicating if object is a DAG stack.}
#'   \item{`validate_stack.dag_stack()`}{The validated
#' DAG stack object.}
#'   \item{`stack_color<-()`}{The modified DAG
#' stack with updated color.}
#'   \item{`as_dag_stack()`}{A `dag_stack` object
#' converted from input.}
#' }
#'
#' @rdname stack
#' @export
new_dag_stack <- function(..., color = blockr.dock::suggest_new_colors()) {
  new_stack(
    ...,
    color = color,
    ctor = "new_dag_stack",
    pkg = pkg_name(),
    class = "dag_stack"
  )
}

#' @param x object.
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

#' @importFrom blockr.dock stack_color
#' @export
stack_color.dag_stack <- function(x) {
  attr(x, "color")
}

#' @param value Replacement value.
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
as_dag_stack.stack <- function(
  x,
  color = blockr.dock::suggest_new_colors(),
  ...
) {
  attr(x, "color") <- color

  class(x) <- c("dag_stack", class(x))

  validate_stack(x)
}

#' @export
as_dag_stack.list <- function(x, ...) {
  do.call(new_dag_stack, x)
}

#' @export
format.dag_stack <- function(x, ...) {
  res <- NextMethod()
  c(res[c(1L, 2L)], paste0("Color: \"", stack_color(x), "\""), res[-c(1L, 2L)])
}

#' @export
as.list.dag_stack <- function(x, ...) {
  list(
    blocks = as.character(x),
    name = stack_name(x),
    color = stack_color(x)
  )
}

#' @export
blockr_deser.dag_stack <- function(x, data, ...) {
  as_dag_stack(data[["payload"]])
}
