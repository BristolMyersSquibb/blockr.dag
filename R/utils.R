graph_id <- function(ns = NULL) {
  res <- "graph"

  if (is.null(ns)) {
    return(res)
  }

  ns(res)
}

last <- function(x) x[[length(x)]]

filter_null <- function(x) Filter(Negate(is.null), x)

has_length <- function(x) length(x) > 0L

# nocov start
preprocess_graph_state <- function() {
  ns <- get_session()$ns
  shiny::snapshotPreprocessInput(
    ns(sprintf("%s-state", graph_id())),
    function(value) {
      strip_positions <- function(elements) {
        lapply(elements, function(el) {
          if (!is.null(el$style)) {
            el$style$x <- NULL
            el$style$y <- NULL
            el$style$z <- NULL
            el$style$src <- "<shinytest2:preprocessed>"
          }
          el
        })
      }

      if (!is.null(value$nodes)) {
        value$nodes <- strip_positions(value$nodes)
      }
      if (!is.null(value$edges)) {
        value$edges <- strip_positions(value$edges)
      }
      if (!is.null(value$combos)) {
        value$combos <- strip_positions(value$combos)
      }
      value
    }
  )
}

preprocess_mouse_position <- function() {
  ns <- get_session()$ns
  shiny::snapshotPreprocessInput(
    ns(sprintf("%s-mouse_position", graph_id())),
    function(value) {
      "<shinytest2:preprocessed>"
    }
  )
} # nocov end

# Resolve a `mod` delta payload from the update-lifecycle reactive
# (blockr.core #175 shape: named list of partial-arg deltas keyed by
# id) against the current board state into a fully-formed S3
# collection.
#
# blockr.core's apply observer does the same merge with the matching
# `update_*()` generic, but we cannot piggy-back on it: that observer
# and `update_observer()` both react to `update()` on the same flush
# with no guaranteed ordering, so reading `board_stacks(board$board)`
# from inside `update_observer()` is not safe to assume reflects the
# merged state. Re-applying the deltas ourselves keeps the consumer
# (combos / nodes / edges renderers) on full objects regardless of
# ordering.
#
# @param deltas Named list of per-id deltas.
# @param current The current collection (e.g. `board_stacks(board)`).
# @param updater The blockr.core `update_*()` generic.
# @param wrap The blockr.core `as_*()` collection coercer.
resolve_mod_deltas <- function(deltas, current, updater, wrap) {
  wrap(Map(updater, current[names(deltas)], deltas))
}

update_action_trigger <- function(action_name, input_name) {
  function(actions, session = get_session()) {
    observeEvent(
      session$input[[input_name]],
      actions[[action_name]](session$input[[input_name]]),
      label = otel_lbl(action_name)
    )
  }
}
