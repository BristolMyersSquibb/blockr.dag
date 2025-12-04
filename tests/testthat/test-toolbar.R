test_that("toolbar default", {
  # Extension
  ext <- new_dag_extension()
  tool <- toolbar_items(ext)

  expect_named(tool, NULL)
  expect_type(tool, "list")

  tst <- build_toolbar(tool)
  expect_type(tst, "character")

  item <- tool[[1]]
  expect_type(item, "list")
})
