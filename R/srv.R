dag_ext_srv <- function(graph) {

	function(id, board, update, ...) {

	  moduleServer(
	    id,
	    function(input, output, session) {

	    	context_menu <- context_menu_items(
	    		isolate(board$board)
	    	)

			  ctx_path <- session$registerDataObj(
			    name = "context-menu-items",
			    data = list(),
			    filterFunc = function(data, req) {
			      body_bytes <- req$rook.input$read(-1)
			      res <- jsonlite::toJSON(
			        build_context_menu(
			          context_menu,
			          board = isolate(board$board),
			          target = jsonlite::fromJSON(rawToChar(body_bytes))
			        )
			      )
			      httpResponse(
			        content_type = "application/json",
			        content = res
			      )
			    }
			  )

        context_menu_entry_action(
        	context_menu,
        	input = input,
        	output = output,
        	session = session,
        	board = board,
        	update = update
        )

				proxy <- init_g6(
					board = isolate(board$board),
					graph = graph,
					path = ctx_path,
					ctx = context_menu,
					session = session
				)

				update_observer(update, proxy)

				reactive(
					input[[paste0(graph_id(), "-state")]]
				)
			}
		)
	}
}

update_observer <- function(update, proxy) {
	observeEvent(
		update(),
		{

		}
	)
}
