library(shinytest2)

create_new_block <- function(app, name, id) {
  app$wait_for_idle()
  do.call(
    app$set_inputs,
    setNames(list(name), "board-add_block_action-add_block_selection")
  )
  app$wait_for_idle()
  app$click(selector = "#board-add_block_action-block-advanced-toggle")
  app$wait_for_idle()

  do.call(
    app$set_inputs,
    setNames(list(id), "board-add_block_action-add_block_id")
  )
  app$wait_for_idle()
  app$click("board-add_block_action-add_block_confirm")
  app$wait_for_idle()
}

create_new_link <- function(app, target, id) {
  app$wait_for_idle()
  do.call(
    app$set_inputs,
    setNames(list(target), "board-add_link_action-create_link")
  )

  app$click(selector = "#board-add_link_action-link-advanced-toggle")

  do.call(
    app$set_inputs,
    setNames(list(id), "board-add_link_action-add_link_id")
  )

  app$click("board-add_link_action-add_link_confirm")
  app$wait_for_idle()
}

right_click <- function(target, app, init = FALSE) {
  # Idea: we target element, get its bounding box, calculate center coordinates
  # and click there with right mouse button. We play with
  # Chromote API ...

  # Do not initialise variables more than once, Chromote
  # does not like it.
  x_result <- if (init) {
    app$get_chromote_session()$Runtime$evaluate(
      expression = sprintf(
        "let target = document.querySelector('%s');
        let rect = target.getBoundingClientRect();
        rect.left + rect.width / 2;",
        target
      )
    )
  } else {
    app$get_chromote_session()$Runtime$evaluate(
      expression = sprintf(
        "target = document.querySelector('%s');
        rect = target.getBoundingClientRect();
        rect.left + rect.width / 2;",
        target
      )
    )
  }

  # Get y coordinate
  y_result <- app$get_chromote_session()$Runtime$evaluate(
    expression = sprintf(
      "rect.top + rect.height / 2;"
    )
  )

  x_coord <- x_result$result$value
  y_coord <- y_result$result$value

  app$get_chromote_session()$Input$dispatchMouseEvent(
    type = "mousePressed",
    x = x_coord,
    y = y_coord,
    button = "right",
    clickCount = 1
  )

  app$get_chromote_session()$Input$dispatchMouseEvent(
    type = "mouseReleased",
    x = x_coord,
    y = y_coord,
    button = "right",
    clickCount = 1
  )

  app$wait_for_idle()
}

expect_values <- function(app, ...) {
  vals <- app$get_values()
  filter_names <- function(x) {
    grep("board-dag_extension", names(x), value = TRUE)
  }
  app$expect_values(
    ...,
    export = vals$export,
    input = filter_names(vals$input),
    output = filter_names(vals$output)
  )
}

test_that("sample_app works", {
  appdir <- system.file(package = "blockr.dag", "examples/empty")

  # when shinytest2 0.5.0 lands ...
  #local_app_support(appdir)

  app <- AppDriver$new(
    appdir,
    name = "empty-app",
    seed = 4323
  )
  expect_values(app)

  # Add a new block: with custom id
  app$wait_for_idle()
  app$click(selector = ".g6-toolbar-item[value=\"add_block\"")
  create_new_block(app, "dataset_block", "super_data_block")
  expect_values(app)

  # Append: show canvas context menu
  # Right-click on canvas to show context menu
  right_click("#board-dag_extension-graph", app, init = TRUE)
  app$click(selector = ".g6-contextmenu-li[value=\"add_block\"]")
  create_new_block(app, "head_block", "super_head_block")
  expect_values(app)

  # Right click on dataset block + add link with head block
  right_click("g#node-super_data_block", app)
  app$click(selector = ".g6-contextmenu-li[value=\"create_link\"]")
  create_new_link(app, "super_head_block", "super_link")
  expect_values(app)

  # Select new block and remove
  app$run_js(
    "HTMLWidgets
      .find('#board-dag_extension-graph')
      .getWidget()
      .setElementState('node-super_data_block', 'selected', false);"
  )
  app$set_inputs(
    `board-dag_extension-graph-selected_node` = "node-super_data_block",
    allow_no_input_binding_ = TRUE
  )
  app$run_js("$(\".g6-contextmenu\").show()")

  # Remove element
  app$click(selector = ".g6-toolbar-item[value=\"remove_selected\"")
  expect_values(app)

  app$stop()
})
