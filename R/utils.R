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

update_action_trigger <- function(action_name, input_name) {
  function(actions, session = get_session()) {
    observeEvent(
      session$input[[input_name]],
      actions[[action_name]](session$input[[input_name]])
    )
  }
}
