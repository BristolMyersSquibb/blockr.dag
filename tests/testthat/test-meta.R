new_dummy_blk <- function(...) {
  new_data_block(
    function(id) {},
    function(id) tagList(),
    class = "test_block",
    ...
  )
}

test_that("blk_category works", {
  blk <- new_dataset_block()
  categ <- blk_category(blk)
  expect_type(categ, "character")

  # Unknown block
  categ <- blk_category(new_dummy_blk())
  expect_identical(categ, default_category())
})

test_that("blk_icon works", {
  blk <- new_dataset_block()
  ic <- blk_icon(blk)
  expect_type(ic, "character")

  # Unknown block
  ic <- blk_icon(new_dummy_blk())
  expect_identical(ic, default_icon(default_category()))
})
