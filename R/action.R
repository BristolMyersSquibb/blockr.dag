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

copy_selection_to_clipboard <- function(board, dag_extension) {
  input <- dag_extension[["proxy"]]$session$input
  selected_nodes <- input[[paste0(graph_id(), "-selected_node")]]
  selected_combos <- input[[paste0(graph_id(), "-selected_combo")]]

  subboard <- extract_subboard(
    board$board,
    block_ids = from_g6_node_id(coal(selected_nodes, character())),
    stack_ids = from_g6_combo_id(coal(selected_combos, character()))
  )

  if (is.null(subboard)) {
    return(NULL)
  }

  states <- live_block_states(board, names(subboard$blocks))

  session <- dag_extension[["proxy"]]$session

  json <- tryCatch(
    as.character(jsonlite::toJSON(
      blockr_ser(subboard, blocks = states),
      auto_unbox = TRUE
    )),
    error = function(e) {
      showNotification(
        paste("Cannot copy selection:", conditionMessage(e)),
        type = "error"
      )
      NULL
    }
  )

  if (is.null(json)) {
    return(NULL)
  }

  session$sendCustomMessage("write-clipboard", list(json = json))

  subboard
}

remove_subboard <- function(subboard, update) {
  update(list(
    blocks = list(rm = names(subboard$blocks)),
    links = list(rm = names(subboard$links)),
    stacks = list(rm = names(subboard$stacks))
  ))
}

copy_selected_action <- function(trigger, board, update, dag_extension, ...) {
  blockr.dock::new_action(
    function(input, output, session) {
      observeEvent(trigger(), {
        copy_selection_to_clipboard(board, dag_extension)
      })
      NULL
    },
    id = "copy_selected_action"
  )
}

cut_selected_action <- function(trigger, board, update, dag_extension, ...) {
  blockr.dock::new_action(
    function(input, output, session) {
      observeEvent(trigger(), {
        subboard <- copy_selection_to_clipboard(board, dag_extension)
        if (!is.null(subboard)) remove_subboard(subboard, update)
      })
      NULL
    },
    id = "cut_selected_action"
  )
}

paste_action <- function(trigger, board, update, dag_extension, ...) {
  blockr.dock::new_action(
    function(input, output, session) {
      observeEvent(trigger(), {
        subboard <- NULL
        tryCatch(
          subboard <- blockr_deser(jsonlite::fromJSON(
            trigger(),
            simplifyDataFrame = FALSE
          )),
          error = function(e) {
            showNotification(
              paste("Cannot paste clipboard content:", conditionMessage(e)),
              type = "error"
            )
          }
        )
        if (is.null(subboard)) {
          return()
        }

        remapped <- remap_subboard_ids(subboard, board$board)

        update(list(
          blocks = list(add = remapped$blocks),
          links = list(add = remapped$links),
          stacks = list(add = remapped$stacks)
        ))
      })
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
