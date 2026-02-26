new_dummy_blk <- function(...) {
  new_data_block(
    function(id) {},
    function(id) tagList(),
    block_metadata = list(),
    class = "test_block",
    ...
  )
}

test_that("blks_color works", {
  blk1 <- new_dataset_block()
  cat1 <- blks_color(blk1)

  expect_type(cat1, "character")
  expect_length(cat1, 1L)

  # Unknown block
  default <- blockr.dock::blk_color(default_category())

  blk2 <- new_dummy_blk()
  cat2 <- blks_color(blk2)

  expect_identical(cat2, default)

  cat3 <- blks_color(c(blk1, blk2))

  expect_type(cat3, "character")
  expect_length(cat3, 2L)
})

test_that("blks_icon works", {
  blk1 <- new_dataset_block()
  icn1 <- blks_icon(blk1)

  expect_type(icn1, "character")
  expect_length(icn1, 1L)

  # Unknown block
  default <- blockr.dock::blk_icon_data_uri(
    default_icon(),
    blockr.dock::blk_color(default_category())
  )

  blk2 <- new_dummy_blk()
  icn2 <- blks_icon(new_dummy_blk())

  expect_identical(icn2, default)

  icn3 <- blks_icon(c(blk1, blk2))

  expect_type(icn3, "character")
  expect_length(icn3, 2L)
})
