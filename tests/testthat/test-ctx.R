test_that("context menu", {

  ext <- new_dag_extension()
  ctx <- context_menu_items(ext)

  node <- build_context_menu(ctx, target = list(type = "node"))

  expect_type(node, "list")
  expect_named(node, NULL)

  expect_setequal(
    chr_xtr(node, "value"),
    c(
      "create_link", "remove_block", "append_block", "edit_inputs", "copy",
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

test_that("only re-targeting entries declare anything", {

  ctx <- context_menu_items(new_dag_extension())
  by_id <- set_names(ctx, chr_ply(ctx, context_menu_entry_id))

  expect_setequal(
    chr_ply(Filter(is_sidebar_entry, ctx), context_menu_entry_id),
    c(
      "create_link", "append_block", "edit_inputs", "edit_stack", "edit_link",
      "insert_block"
    )
  )

  expect_identical(
    sidebar_spec(by_id[["edit_stack"]]),
    list(action = "edit_stack_action")
  )

  expect_identical(
    sidebar_spec(by_id[["edit_link"]]),
    list(action = "edit_link_action")
  )

  expect_identical(
    sidebar_spec(by_id[["edit_inputs"]]),
    list(action = "edit_inputs_action")
  )

  expect_identical(
    sidebar_spec(by_id[["insert_block"]]),
    list(action = "insert_block_action")
  )

  # Panel-filling entries that cannot re-target name nothing at all: the
  # panel they fill is stamped by `show_sidebar()`, not declared here.
  expect_null(sidebar_spec(by_id[["add_block"]]))
  expect_null(sidebar_spec(by_id[["create_stack"]]))
  expect_null(sidebar_spec(by_id[["remove_link"]]))

  expect_length(Filter(is_sidebar_entry, toolbar_items(new_dag_extension())), 0L)
})

test_that("re-target matches the panel owner's concern, only when pinned", {

  ctx <- context_menu_items(new_dag_extension())
  by_id <- set_names(ctx, chr_ply(ctx, context_menu_entry_id))

  stack_editor <- by_id[["edit_stack"]]
  node_editor <- by_id[["create_link"]]
  link_editor <- by_id[["edit_link"]]
  inputs_editor <- by_id[["edit_inputs"]]

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

  expect_true(
    should_retarget(link_editor, NULL, "edge", "e1", pinned = TRUE)
  )
  expect_false(
    should_retarget(link_editor, NULL, "node", "n1", pinned = TRUE)
  )
  expect_false(
    should_retarget(link_editor, NULL, "edge", "e1", pinned = FALSE)
  )

  expect_true(
    should_retarget(inputs_editor, NULL, "node", "n1", pinned = TRUE)
  )
  expect_false(
    should_retarget(inputs_editor, NULL, "edge", "e1", pinned = TRUE)
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
