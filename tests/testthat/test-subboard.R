# --- Fixture ---

test_board <- new_board(
  blocks = c(
    a = new_dataset_block(),
    b = new_head_block(),
    c = new_dataset_block(),
    d = new_scatter_block()
  ),
  links = list(from = "a", to = "b", input = "data"),
  stacks = c(s1 = new_stack(c("a", "b")))
)

# --- new_subboard() ---

test_that("new_subboard() creates a subboard object", {
  sb <- new_subboard(blocks(), links(), stacks())
  expect_s3_class(sb, "subboard")
  expect_named(sb, c("blocks", "links", "stacks"))
  expect_length(sb$blocks, 0L)
  expect_length(sb$links, 0L)
  expect_length(sb$stacks, 0L)
})

# --- is_subboard() ---

test_that("is_subboard() identifies subboard objects", {
  sb <- new_subboard(blocks(), links(), stacks())
  expect_true(is_subboard(sb))
  expect_false(is_subboard(list()))
  expect_false(is_subboard(NULL))
})

# --- subboard_blocks() ---

test_that("subboard_blocks() returns explicit block_ids", {
  all_blocks <- board_blocks(test_board)
  all_stacks <- board_stacks(test_board)

  res <- subboard_blocks(all_blocks, c("a", "c"), character(), all_stacks)
  expect_length(res, 2L)
  expect_named(res, c("a", "c"))
})

test_that("subboard_blocks() resolves stack_ids to blocks", {
  all_blocks <- board_blocks(test_board)
  all_stacks <- board_stacks(test_board)

  res <- subboard_blocks(all_blocks, character(), "s1", all_stacks)
  expect_length(res, 2L)
  expect_true(all(c("a", "b") %in% names(res)))
})

test_that("subboard_blocks() combines block_ids and stack_ids", {
  all_blocks <- board_blocks(test_board)
  all_stacks <- board_stacks(test_board)

  res <- subboard_blocks(all_blocks, "c", "s1", all_stacks)
  expect_length(res, 3L)
  expect_true(all(c("a", "b", "c") %in% names(res)))
})

test_that("subboard_blocks() returns NULL when no match", {
  all_blocks <- board_blocks(test_board)
  all_stacks <- board_stacks(test_board)

  res <- subboard_blocks(all_blocks, "nonexistent", character(), all_stacks)
  expect_null(res)
})

test_that("subboard_blocks() filters out nonexistent IDs", {
  all_blocks <- board_blocks(test_board)
  all_stacks <- board_stacks(test_board)

  res <- subboard_blocks(all_blocks, c("a", "zzz"), character(), all_stacks)
  expect_length(res, 1L)
  expect_named(res, "a")
})

# --- subboard_links() ---

test_that("subboard_links() keeps link when both endpoints selected", {
  all_links <- board_links(test_board)
  res <- subboard_links(all_links, c("a", "b"))
  expect_length(res, 1L)
})

test_that("subboard_links() drops link when one endpoint missing", {
  all_links <- board_links(test_board)
  res <- subboard_links(all_links, "a")
  expect_length(res, 0L)
})

test_that("subboard_links() handles empty all_links", {
  res <- subboard_links(links(), c("a", "b"))
  expect_length(res, 0L)
})

# --- subboard_stacks() ---

test_that("subboard_stacks() returns explicit stack_ids", {
  all_stacks <- board_stacks(test_board)
  res <- subboard_stacks(all_stacks, "s1", character())
  expect_length(res, 1L)
  expect_true("s1" %in% names(res))
})

test_that("subboard_stacks() infers stacks when all member blocks selected", {
  all_stacks <- board_stacks(test_board)
  res <- subboard_stacks(all_stacks, character(), c("a", "b"))
  expect_length(res, 1L)
  expect_true("s1" %in% names(res))
})

test_that("subboard_stacks() does not infer stacks with partial coverage", {
  all_stacks <- board_stacks(test_board)
  res <- subboard_stacks(all_stacks, character(), "a")
  expect_length(res, 0L)
})

# --- extract_subboard() ---

test_that("extract_subboard() with blocks a,b returns subboard", {
  sb <- extract_subboard(test_board, block_ids = c("a", "b"))
  expect_s3_class(sb, "subboard")
  expect_length(sb$blocks, 2L)
  expect_length(sb$links, 1L)
  expect_length(sb$stacks, 1L)
})

test_that("extract_subboard() with stack_ids resolves blocks", {
  sb <- extract_subboard(test_board, stack_ids = "s1")
  expect_s3_class(sb, "subboard")
  expect_length(sb$blocks, 2L)
  expect_true(all(c("a", "b") %in% names(sb$blocks)))
  expect_length(sb$links, 1L)
  expect_length(sb$stacks, 1L)
})

test_that("extract_subboard() with isolated block returns no links/stacks", {
  sb <- extract_subboard(test_board, block_ids = "c")
  expect_s3_class(sb, "subboard")
  expect_length(sb$blocks, 1L)
  expect_length(sb$links, 0L)
  expect_length(sb$stacks, 0L)
})

test_that("extract_subboard() with no matching IDs returns NULL", {
  sb <- extract_subboard(test_board, block_ids = "zzz")
  expect_null(sb)
})

# --- blockr_ser / blockr_deser round-trip ---

test_that("blockr_ser produces expected structure", {
  sb <- extract_subboard(test_board, block_ids = c("a", "b"))
  ser <- blockr_ser(sb)

  expect_identical(ser$object, "subboard")
  expect_true("payload" %in% names(ser))
  expect_true(all(c("blocks", "links", "stacks") %in% names(ser$payload)))
})

test_that("blockr_ser / blockr_deser round-trip preserves structure", {
  sb <- extract_subboard(test_board, block_ids = c("a", "b"))
  ser <- blockr_ser(sb)
  deser <- blockr_deser(ser)

  expect_s3_class(deser, "subboard")
  expect_length(deser$blocks, length(sb$blocks))
  expect_length(deser$links, length(sb$links))
  expect_length(deser$stacks, length(sb$stacks))
})

test_that("full JSON pipeline round-trips", {
  sb <- extract_subboard(test_board, block_ids = c("a", "b"))
  json <- jsonlite::toJSON(blockr_ser(sb), auto_unbox = TRUE)
  data <- jsonlite::fromJSON(as.character(json), simplifyDataFrame = FALSE)
  deser <- blockr_deser(data)

  expect_s3_class(deser, "subboard")
  expect_length(deser$blocks, 2L)
  expect_length(deser$links, 1L)
  expect_length(deser$stacks, 1L)
})

test_that("empty subboard round-trips", {
  sb <- new_subboard(blocks(), links(), stacks())
  ser <- blockr_ser(sb)
  deser <- blockr_deser(ser)

  expect_s3_class(deser, "subboard")
  expect_length(deser$blocks, 0L)
  expect_length(deser$links, 0L)
  expect_length(deser$stacks, 0L)
})

# --- remap_blocks() ---

test_that("remap_blocks() remaps IDs and adds (copy) suffix", {
  sb <- extract_subboard(test_board, block_ids = c("a", "b"))
  id_map <- c(a = "x1", b = "x2")

  remapped <- remap_blocks(sb$blocks, id_map)
  expect_named(remapped, c("x1", "x2"))
  expect_true(all(grepl(
    "\\(copy\\)$",
    vapply(
      as.list(remapped),
      block_name,
      character(1)
    )
  )))
})

# --- remap_links() ---

test_that("remap_links() remaps from/to and generates fresh IDs", {
  sb <- extract_subboard(test_board, block_ids = c("a", "b"))
  block_id_map <- c(a = "x1", b = "x2")
  used_ids <- c("a", "b", "x1", "x2", names(sb$links))

  remapped <- remap_links(sb$links, block_id_map, used_ids)
  expect_length(remapped, length(sb$links))

  lnk <- as.list(remapped)[[1]]
  expect_identical(lnk$from, "x1")
  expect_identical(lnk$to, "x2")

  expect_true(!any(names(remapped) %in% used_ids))
})

test_that("remap_links() handles empty links", {
  res <- remap_links(links(), c(a = "x1"), c("a", "x1"))
  expect_length(res, 0L)
})

# --- remap_stacks() ---

test_that("remap_stacks() remaps block refs and adds (copy) suffix", {
  sb <- extract_subboard(test_board, block_ids = c("a", "b"))
  block_id_map <- c(a = "x1", b = "x2")
  used_ids <- c("a", "b", "x1", "x2", "s1")

  remapped <- remap_stacks(sb$stacks, block_id_map, used_ids)
  expect_length(remapped, 1L)

  stk <- as.list(remapped)[[1]]
  expect_true(all(stack_blocks(stk) %in% c("x1", "x2")))
  expect_true(grepl("\\(copy\\)$", stack_name(stk)))

  expect_true(!any(names(remapped) %in% used_ids))
})

# --- remap_subboard_ids() ---

test_that("remap_subboard_ids() produces no collisions", {
  sb <- extract_subboard(test_board, block_ids = c("a", "b"))
  remapped <- remap_subboard_ids(sb, test_board)

  orig_ids <- c(
    board_block_ids(test_board),
    board_link_ids(test_board),
    board_stack_ids(test_board)
  )

  expect_true(!any(names(remapped$blocks) %in% orig_ids))
  expect_true(!any(names(remapped$links) %in% orig_ids))
  expect_true(!any(names(remapped$stacks) %in% orig_ids))
})

test_that("remap_subboard_ids() preserves counts", {
  sb <- extract_subboard(test_board, block_ids = c("a", "b"))
  remapped <- remap_subboard_ids(sb, test_board)

  expect_length(remapped$blocks, length(sb$blocks))
  expect_length(remapped$links, length(sb$links))
  expect_length(remapped$stacks, length(sb$stacks))
})

test_that("remap_subboard_ids() adds (copy) suffix to block names", {
  sb <- extract_subboard(test_board, block_ids = c("a", "b"))
  remapped <- remap_subboard_ids(sb, test_board)

  nms <- vapply(as.list(remapped$blocks), block_name, character(1))
  expect_true(all(grepl("\\(copy\\)$", nms)))
})

test_that("remap_subboard_ids() links reference new block IDs", {
  sb <- extract_subboard(test_board, block_ids = c("a", "b"))
  remapped <- remap_subboard_ids(sb, test_board)

  new_block_ids <- names(remapped$blocks)
  lnk <- as.list(remapped$links)[[1]]
  expect_true(lnk$from %in% new_block_ids)
  expect_true(lnk$to %in% new_block_ids)
})

test_that("remap_subboard_ids() stack block refs are subset of new block IDs", {
  sb <- extract_subboard(test_board, block_ids = c("a", "b"))
  remapped <- remap_subboard_ids(sb, test_board)

  new_block_ids <- names(remapped$blocks)
  stk <- as.list(remapped$stacks)[[1]]
  expect_true(all(stack_blocks(stk) %in% new_block_ids))
})
