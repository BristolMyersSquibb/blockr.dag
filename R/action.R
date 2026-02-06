draw_link_action <- function(trigger, board, update, dag_extension, ...) {
  blockr.dock::new_action(
    function(input, output, session) {
      proxy <- blockr_g6_proxy(session)
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

            return()
          }

          new_lnk <- new_link(
            from = new$source,
            to = new$target,
            input = inps[1L]
          )

          update(list(links = list(add = as_links(new_lnk))))
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
