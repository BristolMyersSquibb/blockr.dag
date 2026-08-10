test_that("context menu", {

  ext <- new_dag_extension()
  ctx <- context_menu_items(ext)

  node <- build_context_menu(ctx, target = list(type = "node"))

  expect_type(node, "list")
  expect_named(node, NULL)

  expect_setequal(
    chr_xtr(node, "value"),
    c("create_link", "remove_block", "append_block", "copy", "cut")
  )

  edge <- build_context_menu(ctx, target = list(type = "edge"))

  expect_type(edge, "list")
  expect_named(edge, NULL)

  expect_setequal(
    chr_xtr(edge, "value"),
    "remove_link"
  )

  canv <- build_context_menu(ctx, target = list(type = "canvas"))

  expect_type(canv, "list")
  expect_named(canv, NULL)

  expect_setequal(
    chr_xtr(canv, "value"),
    c("create_stack", "add_block", "paste")
  )

  comb <- build_context_menu(ctx, target = list(type = "combo"))

  expect_type(comb, "list")
  expect_named(comb, NULL)

  expect_setequal(
    chr_xtr(comb, "value"),
    c("remove_stack", "edit_stack", "copy", "cut")
  )
})

test_that("sidebar entries declare the panel they fill", {

  ctx <- context_menu_items(new_dag_extension())
  by_id <- set_names(ctx, chr_ply(ctx, context_menu_entry_id))

  expect_setequal(
    chr_ply(Filter(is_sidebar_entry, ctx), context_menu_entry_id),
    c("create_link", "append_block", "edit_stack", "add_block", "create_stack")
  )

  expect_false(is_sidebar_entry(by_id[["remove_block"]]))
  expect_false(is_sidebar_entry(by_id[["remove_link"]]))
  expect_false(is_sidebar_entry(by_id[["copy"]]))

  expect_identical(
    sidebar_spec(by_id[["edit_stack"]]),
    list(
      panel = "actions_sidebar",
      input = "ctx_edit_stack",
      action = "edit_stack_action"
    )
  )

  expect_identical(
    sidebar_panel(by_id[["append_block"]]),
    "append_block_sidebar"
  )

  expect_identical(
    sidebar_spec(by_id[["create_stack"]]),
    list(panel = "actions_sidebar", input = "ctx_create_stack", action = NULL)
  )
})

test_that("only re-targeting entries claim a panel", {

  ctx <- context_menu_items(new_dag_extension())
  by_id <- set_names(ctx, chr_ply(ctx, context_menu_entry_id))

  expect_identical(
    sidebar_claim(by_id[["edit_stack"]]),
    by_id[["edit_stack"]]
  )

  expect_null(sidebar_claim(by_id[["create_stack"]]))
  expect_null(sidebar_claim(by_id[["add_block"]]))

  tools <- toolbar_items(new_dag_extension())
  by_tool <- set_names(tools, chr_ply(tools, toolbar_item_id))

  expect_setequal(
    chr_ply(Filter(is_sidebar_entry, tools), toolbar_item_id),
    c("add_block", "add_stack")
  )

  expect_identical(
    sidebar_spec(by_tool[["add_stack"]]),
    list(panel = "actions_sidebar", input = "tool_add_stack", action = NULL)
  )

  expect_null(sidebar_claim(by_tool[["add_stack"]]))
})

test_that("re-target matches the panel owner's concern, only when pinned", {

  ctx <- context_menu_items(new_dag_extension())
  by_id <- set_names(ctx, chr_ply(ctx, context_menu_entry_id))

  stack_editor <- by_id[["edit_stack"]]
  node_editor <- by_id[["create_link"]]

  expect_true(
    should_retarget(stack_editor, NULL, "combo", "s1", pinned = TRUE)
  )
  expect_false(
    should_retarget(stack_editor, NULL, "node", "n1", pinned = TRUE)
  )

  expect_true(
    should_retarget(node_editor, NULL, "node", "n1", pinned = TRUE)
  )
  expect_false(
    should_retarget(node_editor, NULL, "edge", "e1", pinned = TRUE)
  )

  expect_false(
    should_retarget(stack_editor, NULL, "combo", "s1", pinned = FALSE)
  )
  expect_false(
    should_retarget(stack_editor, NULL, "combo", c("s1", "s2"), TRUE)
  )
  expect_false(
    should_retarget(NULL, NULL, "combo", "s1", pinned = TRUE)
  )
})
