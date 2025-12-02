draw_link_action <- blockr.dock::new_action(
  {
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

          remove_edges(new$id, asis = TRUE, proxy = proxy)

          return()
        }        

        remove_edges(new$id, asis = TRUE, proxy = proxy)

        new_lnk <- new_link(
          from = new$source,
          to = new$target,
          input = inps[1L]
        )

        update(list(links = list(add = as_links(new_lnk))))
      }
    )
  }
)

remove_selected_action <- blockr.dock::new_action(
  {
    observeEvent(
      trigger(),
      {
        input <- session$input
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
  }
)
