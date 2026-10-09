#' DAG board
#'
#' A board drawn as a single canvas: the DAG is the whole front end, and each
#' block is a node of it carrying the block's card (its controls and output),
#' with links between the nodes' ports. It extends [blockr.dock::new_dock_board()],
#' so the board model, its actions, sidebars, extensions and serialization are
#' the dock's; only the front end differs. The page carries no Bootstrap: it is
#' styled by blockr.ui.
#'
#' @inheritParams blockr.dock::new_dock_board
#' @param extensions Dock extensions. The DAG extension draws the canvas, so it
#'   is always included.
#'
#' @return `new_dag_board()` returns a board inheriting from `dag_board` and
#'   `dock_board`; `is_dag_board()` returns a boolean.
#'
#' @examples
#' brd <- new_dag_board(
#'   blocks = c(
#'     a = blockr.core::new_dataset_block("iris"),
#'     b = blockr.core::new_head_block()
#'   ),
#'   links = c(ab = blockr.core::new_link("a", "b"))
#' )
#' is_dag_board(brd)
#'
#' if (interactive()) {
#'   blockr.core::serve(brd)
#' }
#'
#' @export
new_dag_board <- function(blocks = list(), links = list(), stacks = list(),
                          ..., extensions = new_dag_extension(),
                          ctor = NULL, pkg = NULL, class = character()) {

  extensions <- blockr.dock::as_dock_extensions(extensions)

  if (!any(vapply(as.list(extensions), inherits, logical(1L),
                  "dag_extension"))) {
    extensions <- blockr.dock::as_dock_extensions(
      c(as.list(extensions), list(new_dag_extension()))
    )
  }

  blockr.dock::new_dock_board(
    blocks = blocks,
    links = links,
    stacks = stacks,
    ...,
    extensions = extensions,
    ctor = blockr.core::forward_ctor(ctor),
    pkg = pkg,
    class = c(class, "dag_board")
  )
}

#' @rdname new_dag_board
#' @param x Object
#' @export
is_dag_board <- function(x) {
  inherits(x, "dag_board")
}

#' @export
blockr_app_ui.dag_board <- function(id, x, plugins, options, ...,
                                    query = list()) {
  htmltools::tagList(
    htmltools::tags$head(htmltools::tags$title("blockr")),
    board_ui(id, x, plugins, options = options),
    ...
  )
}

#' @export
board_ui.dag_board <- function(id, x, plugins = board_plugins(x),
                               options = blockr.core::blockr_app_options(x),
                               ...) {

  stopifnot(is_string(id))

  dag <- dag_extension_id(x)

  htmltools::tagList(
    blockr.ui::theme_dep(),
    blockr.ui::base_dep(),
    blockr.ui::controls_dep(),
    dock_card_deps(),
    dag_board_dep(),
    shinyjs::useShinyjs(),
    if ("notify_user" %in% names(plugins)) {
      board_ui(id, plugins[["notify_user"]], x)
    },
    htmltools::div(
      class = "blockr-dag-board",
      `data-card-gap` = card_gap(),
      `data-on-screen-input` = NS(id, "on_screen"),
      blockr.dock::extension_ui(
        blockr.dock::dock_extensions(x)[[dag]],
        dag,
        id,
        board = x
      )
    ),
    dock_internal("sidebar_ui")(
      NS(id, "actions_sidebar"),
      mode = "overlay",
      side = "right"
    )
  )
}

#' @export
blockr_app_server.dag_board <- function(id, x, plugins, options, ...,
                                        query = list()) {

  callback <- function(...) {
    dag_board_callback(..., plugins = plugins)
  }

  board_server(id, x, plugins, options, callbacks = callback,
               callback_location = "start", ...)
}

# The block cards are the nodes' content, added and removed with the nodes by
# the DAG extension, so there is no separate block UI to insert or remove.

#' @export
insert_block_ui.dag_board <- function(id, x, blocks = NULL, ...,
                                      session = get_session()) {
  invisible(x)
}

#' @export
remove_block_ui.dag_board <- function(id, x, blocks = NULL, ...,
                                      session = get_session()) {
  invisible(x)
}

# The board server callback of a DAG board: the front-end independent half of
# blockr.dock's board_server_callback(). It runs the extension servers and
# registers the board's and extensions' actions; there are no dock views to
# reconcile. The board is lazy: it evaluates the blocks whose cards are on
# screen, and what feeds them (see hold_on_screen()).
dag_board_callback <- function(board, update, visibility, ..., session,
                               plugins) {

  initial_board <- isolate(board$board)

  exts <- as.list(blockr.dock::dock_extensions(initial_board))

  actions <- unlist(
    c(
      list(board_actions(initial_board)),
      lapply(exts, board_actions)
    ),
    recursive = FALSE
  )

  triggers <- blockr.dock::action_triggers(actions)

  dock_internal("freeze_hidden_inputs")(board, visibility)

  peers <- new.env(parent = emptyenv())

  for (ext_id in names(exts)) {
    local({
      eid <- ext_id
      makeActiveBinding(eid, function() ext_res[[eid]], peers)
    })
  }

  ext_res <- set_names(
    Map(
      function(ext, key) {
        # argument lists, as blockr.dock passes them: extension_server()
        # concatenates them into the server's arguments
        blockr.dock::extension_server(
          ext,
          key,
          list(
            board = board,
            update = update,
            actions = triggers,
            extensions = peers,
            plugins = plugins
          ),
          list(...)
        )
      },
      exts,
      names(exts)
    ),
    names(exts)
  )

  observeEvent(
    update()$extensions$mod,
    dock_internal("apply_extensions_mod")(update()$extensions$mod, ext_res)
  )

  dock_internal("register_actions")(actions, triggers, board, update, ext_res)

  owner <- session$ns("canvas")
  hold_on_screen(session$input, update, visibility, owner)

  list(
    dock = NULL,
    actions = triggers,
    view_data = NULL,
    extensions = ext_res,
    eager = blockr.core::eager(owner)
  )
}

dag_extension_id <- function(board) {
  blockr.dock::extension_ids(board, "dag_extension")[1L]
}

dag_board_dep <- function() {
  htmltools::htmlDependency(
    name = "dag-board",
    version = pkg_version(),
    src = c(file = "assets"),
    stylesheet = file.path("css", "dag-board.css"),
    script = file.path("js", c("make-room.js", "on-screen.js")),
    package = pkg_name()
  )
}

# The stylesheets and scripts of blockr.dock's block card: the card itself,
# its title rename and block menu. Its tooltips are `data-blockr-tooltip`
# attributes, which `blockr.ui::controls_dep()` shows.
dock_card_deps <- function() {
  lapply(
    c(
      "blockr_dock_dep", "show_block_dep", "block_rename_dep",
      "add_block_menu_dep"
    ),
    function(fn) dock_internal(fn)()
  )
}

# blockr.dock functions the DAG board uses that blockr.dock does not export.
# One place, so that the list of what would need exporting stays visible.
dock_internal <- function(fn) {
  utils::getFromNamespace(fn, "blockr.dock")
}
