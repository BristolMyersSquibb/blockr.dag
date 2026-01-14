draw_link_action <- function(trigger, board, update, dag_extension, ...) {
  blockr.dock::new_action(
    function(input, output, session) {
      observeEvent(
        trigger(),
        {
          new <- trigger()

          blocks <- board_blocks(board$board)

          inps <- blockr.dock::block_input_select(
            blocks[[new$target]],
            new$target,
            board_links(board$board),
            mode = "inputs"
          )

          if (length(inps) == 0L) {
            notify(
              "No inputs are available for block {new$target}.",
              type = "warning"
            )

            remove_edges(new$id, asis = TRUE, proxy = dag_extension[["proxy"]])

            return()
          }

          remove_edges(new$id, asis = TRUE, proxy = dag_extension[["proxy"]])

          new_lnk <- as_links(
            new_link(
              from = new$source,
              to = new$target,
              input = inps[1L]
            )
          )
          # Pass the sourcePort for g6_edges_from_links
          attr(new_lnk$from, "port") <- new$sourcePort

          update(list(links = list(add = new_lnk)))
        }
      )

      NULL
    },
    id = "draw_link_action"
  )
}

remove_selected_action <- function(trigger, board, update, dag_extension, ...) {
  blockr.dock::new_action(
    function(input, output, session) {
      input <- dag_extension[["proxy"]]$session$input
      observeEvent(
        trigger(),
        {
          update(
            list(
              blocks = list(
                rm = from_g6_node_id(
                  input[[paste0(graph_id(), "-selected_node")]]
                )
              ),
              links = list(
                rm = from_g6_edge_id(
                  input[[paste0(graph_id(), "-selected_edge")]]
                )
              ),
              stacks = list(
                rm = from_g6_combo_id(
                  input[[paste0(graph_id(), "-selected_combo")]]
                )
              )
            )
          )
        }
      )

      NULL
    },
    id = "remove_selected_action"
  )
}

#' @importFrom blockr.dock board_actions
#' @export
board_actions.dag_extension <- function(x, ...) {
  list(
    draw_link_action,
    remove_selected_action
  )
}
