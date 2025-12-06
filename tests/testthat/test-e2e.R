library(shinytest2)
test_that("sample_app works", {
  appdir <- system.file(package = "blockr.dag", "examples/empty")

  # when shinytest2 0.5.0 lands ...
  #local_app_support(appdir)

  app <- AppDriver$new(appdir, name = "empty-app", seed = 4323)

  app$expect_values()

  # Add a new block: with custom id
  app$click(selector = ".g6-toolbar-item[value=\"add_block\"")
  app$wait_for_idle()
  app$set_inputs(
    `board-dag_extension-tool_add_block-add_block_selection` = "dataset_block"
  )
  app$wait_for_idle()
  app$click(
    selector = "#board-dag_extension-tool_add_block-block-advanced-toggle"
  )
  app$wait_for_idle()
  app$set_inputs(
    `board-dag_extension-tool_add_block-add_block_id` = "super_data_block"
  )
  app$wait_for_idle()
  app$click("board-dag_extension-tool_add_block-add_block_confirm")
  app$expect_values()

  # TBD: continue testing more interactions

  app$stop()
})
