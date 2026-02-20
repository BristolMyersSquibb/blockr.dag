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

copy_selected_action <- function(trigger, board, update, dag_extension, ...) {
  blockr.dock::new_action(
    function(input, output, session) {
      input <- dag_extension[["proxy"]]$session$input
      observeEvent(
        trigger(),
        {
          selected_nodes <- input[[paste0(graph_id(), "-selected_node")]]
          selected_combos <- input[[paste0(graph_id(), "-selected_combo")]]

          subgraph <- extract_subgraph(
            board$board,
            coal(selected_nodes, character()),
            coal(selected_combos, character())
          )

          if (is.null(subgraph)) {
            return()
          }

          states <- live_block_states(board, names(subgraph$blocks))

          json <- as.character(jsonlite::toJSON(
            blockr_ser_subgraph(subgraph, block_states = states),
            auto_unbox = TRUE
          ))

          session <- dag_extension[["proxy"]]$session
          session$sendCustomMessage("write-clipboard", list(json = json))
        }
      )

      NULL
    },
    id = "copy_selected_action"
  )
}

cut_selected_action <- function(trigger, board, update, dag_extension, ...) {
  blockr.dock::new_action(
    function(input, output, session) {
      input <- dag_extension[["proxy"]]$session$input
      observeEvent(
        trigger(),
        {
          selected_nodes <- input[[paste0(graph_id(), "-selected_node")]]
          selected_combos <- input[[paste0(graph_id(), "-selected_combo")]]

          subgraph <- extract_subgraph(
            board$board,
            coal(selected_nodes, character()),
            coal(selected_combos, character())
          )

          if (is.null(subgraph)) {
            return()
          }

          states <- live_block_states(board, names(subgraph$blocks))

          json <- as.character(jsonlite::toJSON(
            blockr_ser_subgraph(subgraph, block_states = states),
            auto_unbox = TRUE
          ))

          session <- dag_extension[["proxy"]]$session
          session$sendCustomMessage("write-clipboard", list(json = json))

          # Remove the selected elements
          update(
            list(
              blocks = list(rm = names(subgraph$blocks)),
              links = list(rm = names(subgraph$links)),
              stacks = list(rm = names(subgraph$stacks))
            )
          )
        }
      )

      NULL
    },
    id = "cut_selected_action"
  )
}

paste_action <- function(trigger, board, update, dag_extension, ...) {
  blockr.dock::new_action(
    function(input, output, session) {
      observeEvent(
        trigger(),
        {
          tryCatch(
            {
              data <- jsonlite::fromJSON(trigger(), simplifyDataFrame = FALSE)
              subgraph <- blockr_deser_subgraph(data)
              remapped <- remap_subgraph_ids(subgraph, board$board)

              update(
                list(
                  blocks = list(add = remapped$blocks),
                  links = list(add = remapped$links),
                  stacks = list(add = remapped$stacks)
                )
              )
            },
            error = function(e) {
              # Silently no-op on bad data
            }
          )
        }
      )

      NULL
    },
    id = "paste_action"
  )
}

#' @importFrom blockr.dock board_actions
#' @export
board_actions.dag_extension <- function(x, ...) {
  list(
    draw_link_action,
    remove_selected_action,
    copy_selected_action,
    cut_selected_action,
    paste_action
  )
}
