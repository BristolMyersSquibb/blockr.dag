draw_link_action <- function(trigger, board, update, dag_extension, ...) {
  blockr.dock::new_action(
    function(input, output, session) {
      observeEvent(
        trigger(),
        {
          new <- trigger()

          remove_edges(new$id, asis = TRUE, proxy = dag_extension[["proxy"]])

          target_id <- new$target
          source_id <- new$source
          input_name <- from_g6_port_id(
            new$targetPort,
            to_g6_node_id(new$target)
          )

          # For variadic blocks, make input name unique
          target_block <- board_blocks(board$board)[[target_id]]
          if (is.na(blockr.core::block_arity(target_block))) {
            existing <- board_links(board$board)
            n_existing <- sum(existing$to == target_id)
            if (n_existing > 0) {
              input_name <- paste0(input_name, "_", n_existing + 1)
            }
          }
          new_lnk <- as_links(
            new_link(
              from = source_id,
              to = target_id,
              input = input_name
            )
          )

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
