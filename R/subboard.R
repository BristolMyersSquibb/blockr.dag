# --- Constructor ---

new_subboard <- function(
  blocks = blocks(),
  links = links(),
  stacks = stacks()
) {
  structure(
    list(blocks = blocks, links = links, stacks = stacks),
    class = "subboard"
  )
}

is_subboard <- function(x) {
  inherits(x, "subboard")
}

# --- Extraction helpers ---

subboard_blocks <- function(all_blocks, block_ids, stack_ids, all_stacks) {
  for (sid in intersect(stack_ids, names(all_stacks))) {
    block_ids <- union(block_ids, stack_blocks(all_stacks[[sid]]))
  }
  block_ids <- intersect(block_ids, names(all_blocks))
  if (length(block_ids) == 0L) {
    return(NULL)
  }
  all_blocks[block_ids]
}

subboard_links <- function(all_links, block_ids) {
  if (length(all_links) == 0L) {
    return(links())
  }
  keep <- all_links$from %in% block_ids & all_links$to %in% block_ids
  if (any(keep)) all_links[keep] else links()
}

subboard_stacks <- function(all_stacks, stack_ids, block_ids) {
  selected <- intersect(stack_ids, names(all_stacks))
  candidates <- setdiff(names(all_stacks), selected)
  inferred <- Filter(
    function(sid) {
      blks <- stack_blocks(all_stacks[[sid]])
      length(blks) > 0L && all(blks %in% block_ids)
    },
    candidates
  )
  all_stacks[union(selected, inferred)]
}

extract_subboard <- function(
  board,
  block_ids = character(),
  stack_ids = character()
) {
  stopifnot(
    is_board(board),
    is.character(block_ids),
    is.character(stack_ids)
  )

  all_blocks <- board_blocks(board)
  all_stacks <- board_stacks(board)

  blocks <- subboard_blocks(all_blocks, block_ids, stack_ids, all_stacks)
  if (is.null(blocks)) {
    return(NULL)
  }

  block_ids <- names(blocks)

  new_subboard(
    blocks = blocks,
    links = subboard_links(board_links(board), block_ids),
    stacks = subboard_stacks(all_stacks, stack_ids, block_ids)
  )
}

# --- S3 ser/deser methods ---

#' @exportS3Method blockr.core::blockr_ser subboard
blockr_ser.subboard <- function(x, blocks = NULL, ...) {
  list(
    object = class(x),
    payload = list(
      blocks = blockr_ser(x$blocks, blocks = blocks),
      links = blockr_ser(x$links),
      stacks = blockr_ser(x$stacks)
    )
  )
}

#' @exportS3Method blockr.core::blockr_deser subboard
blockr_deser.subboard <- function(x, data, ...) {
  new_subboard(
    blocks = blockr_deser(data$payload$blocks),
    links = blockr_deser(data$payload$links),
    stacks = blockr_deser(data$payload$stacks)
  )
}

# --- Live state capture ---

live_block_states <- function(board, block_ids) {
  stopifnot(is.character(block_ids))
  running <- board$blocks

  ids <- intersect(block_ids, names(running))
  lapply(
    lst_xtr(running[ids], "server", "state"),
    lapply,
    reval_if
  )
}

# --- ID remapping helpers ---

remap_blocks <- function(blocks, id_map) {
  blk_list <- as.list(blocks)
  names(blk_list) <- id_map[names(blk_list)]
  blk_list <- lapply(blk_list, function(blk) {
    block_name(blk) <- paste0(block_name(blk), " (copy)")
    blk
  })
  as_blocks(blk_list)
}

remap_links <- function(lnks, block_id_map, used_ids) {
  if (length(lnks) == 0L) {
    return(links())
  }
  remapped <- lapply(as.list(lnks), function(lnk) {
    new_link(
      from = unname(block_id_map[lnk$from]),
      to = unname(block_id_map[lnk$to]),
      input = lnk$input
    )
  })
  names(remapped) <- rand_names(old_names = used_ids, n = length(remapped))
  do.call(links, remapped)
}

remap_stacks <- function(stks, block_id_map, used_ids) {
  if (length(stks) == 0L) {
    return(stacks())
  }
  remapped <- lapply(as.list(stks), function(stk) {
    blockr.dock::new_dock_stack(
      blocks = unname(block_id_map[stack_blocks(stk)]),
      name = paste0(stack_name(stk), " (copy)"),
      color = blockr.dock::stack_color(stk)
    )
  })
  names(remapped) <- rand_names(old_names = used_ids, n = length(remapped))
  as_stacks(remapped)
}

remap_subboard_ids <- function(subboard, board) {
  stopifnot(
    is_subboard(subboard),
    is_board(board)
  )

  used_ids <- c(
    board_block_ids(board),
    board_link_ids(board),
    board_stack_ids(board)
  )

  new_block_ids <- rand_names(
    old_names = used_ids,
    n = length(subboard$blocks)
  )
  block_id_map <- set_names(new_block_ids, names(subboard$blocks))
  used_ids <- c(used_ids, new_block_ids)

  blocks <- remap_blocks(subboard$blocks, block_id_map)

  lnks <- remap_links(subboard$links, block_id_map, used_ids)
  used_ids <- c(used_ids, names(lnks))

  stks <- remap_stacks(subboard$stacks, block_id_map, used_ids)

  new_subboard(blocks, lnks, stks)
}
