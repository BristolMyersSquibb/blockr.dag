# The Workflow extension's model-facing metadata. `blockr.dock`'s
# external-control tooling reads each field on its own, so what matters is
# that the right thing lands in the right slot rather than that some blob
# mentions it all.

test_that("the extension carries structured metadata", {

  ext <- new_dag_extension()

  expect_true(blockr.dock::is_ext_meta(blockr.dock::ext_meta(ext)))

  # What the view is, and nothing about how to drive it.
  desc <- blockr.dock::ext_desc(ext)
  expect_match(desc, "Workflow diagram")
  expect_no_match(desc, "modify_extension")
  expect_no_match(desc, "positions")
})

test_that("the positions schema documents the one controllable variable", {

  args <- blockr.dock::ext_args(new_dag_extension())

  expect_named(args, "positions")

  spec <- args[["positions"]]
  desc <- blockr.core::arg_spec_description(spec)

  # The schema and the relative-placement arithmetic, which a model cannot
  # infer from the variable name.
  expect_match(desc, "canvas-pixel coordinates")
  expect_match(desc, "150px between centres")

  # The outer map is block-id keyed, which the closed `arg_object()` record
  # cannot express, so the shape travels as an example instead.
  example <- blockr.core::arg_spec_example(spec)
  expect_type(example, "list")
  expect_named(example[[1L]], c("x", "y"))
})

test_that("the steering is guidance, not description", {

  ext <- new_dag_extension()
  guide <- blockr.dock::ext_guidance(ext)

  expect_match(guide, "modify_extension")
  expect_match(guide, "never")

  # A worked payload, shaped as `modify_extension` takes it.
  examples <- blockr.dock::ext_examples(ext)
  expect_length(examples, 1L)
  expect_named(examples[[1L]], "positions")
})

test_that("metadata is orthogonal to the access gate", {

  # #359 keeps `external_ctrl` as what may be driven; the metadata only says
  # how. Documenting `positions` must not be what grants access to it.
  expect_identical(attr(new_dag_extension(), "external_ctrl"), "positions")
})
