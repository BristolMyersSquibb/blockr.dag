test_that("context menu", {

  ext <- new_dag_extension()
  ctx <- context_menu_items(ext)

  node <- build_context_menu(ctx, target = list(type = "node"))

  expect_type(node, "list")
  expect_named(node, NULL)

  expect_setequal(
    chr_xtr(node, "value"),
    c(
      "create_link", "remove_block", "append_block", "add_to_stack", "copy",
      "cut"
    )
  )

  edge <- build_context_menu(ctx, target = list(type = "edge"))

  expect_type(edge, "list")
  expect_named(edge, NULL)

  expect_setequal(
    chr_xtr(edge, "value"),
    c("remove_link", "edit_link", "insert_block")
  )

  canv <- build_context_menu(ctx, target = list(type = "canvas"))

  expect_type(canv, "list")
  expect_named(canv, NULL)

  expect_setequal(
    chr_xtr(canv, "value"),
    c("add_block", "paste")
  )

  comb <- build_context_menu(ctx, target = list(type = "combo"))

  expect_type(comb, "list")
  expect_named(comb, NULL)

  expect_setequal(
    chr_xtr(comb, "value"),
    c("remove_stack", "edit_stack", "copy", "cut")
  )
})

test_that("the right-click entries open the dock's menus", {

  ctx <- context_menu_items(new_dag_extension())
  ids <- chr_ply(ctx, context_menu_entry_id)
  by_id <- set_names(ctx, ids)

  # The sidebar forms are gone (blockr.dock#544): Edit inputs is covered by
  # the link menu and Connect, and a stack is made from blocks.
  expect_false(any(c("edit_inputs", "create_stack") %in% ids))
  expect_true("add_to_stack" %in% ids)

  expect_identical(context_menu_entry_name(by_id[["create_link"]]), "Connect to\u2026")
  expect_identical(context_menu_entry_name(by_id[["remove_stack"]]), "Dissolve stack")

  # The entries that open a menu say where the click was.
  for (id in c("create_link", "edit_link", "edit_stack", "add_to_stack")) {
    js <- by_id[[id]]$js(function(x) paste0("ns-", x))
    expect_match(js, "at: {x: box.left, y: box.bottom}", fixed = TRUE, info = id)
  }

  # Add to stack takes the selection when the block is one of several selected.
  js <- by_id[["add_to_stack"]]$js(function(x) paste0("ns-", x))
  expect_match(js, "getElementDataByState('node', 'selected')", fixed = TRUE)
  expect_match(js, "ns-graph", fixed = TRUE)
})

test_that("retarget is accepted and ignored", {
  entry <- new_context_menu_entry("Edit", "() => {}", retarget = TRUE)
  expect_null(attr(entry, "sidebar"))
})
