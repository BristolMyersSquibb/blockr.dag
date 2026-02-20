extract_subgraph <- function(board, selected_nodes, selected_combos) {
  all_blocks <- board_blocks(board)
  all_links <- board_links(board)
  all_stacks <- board_stacks(board)

  block_ids <- from_g6_node_id(selected_nodes)
  combo_ids <- from_g6_combo_id(selected_combos)

  # Expand selected combos -> add their block IDs
  for (sid in combo_ids) {
    if (sid %in% names(all_stacks)) {
      block_ids <- union(block_ids, stack_blocks(all_stacks[[sid]]))
    }
  }

  # Filter to blocks that actually exist
  block_ids <- intersect(block_ids, names(all_blocks))

  if (length(block_ids) == 0L) {
    return(NULL)
  }

  # Subset blocks
  sub_blocks <- all_blocks[block_ids]

  # Filter links: keep only where both from and to are in selection
  sub_links <- links()
  if (length(all_links) > 0L) {
    keep <- all_links$from %in% block_ids & all_links$to %in% block_ids
    if (any(keep)) {
      sub_links <- all_links[keep]
    }
  }

  # Stack inference: include stack if ALL its blocks are in selection
  sub_stacks <- stacks()
  if (length(all_stacks) > 0L) {
    stk_ids <- character()
    for (sid in names(all_stacks)) {
      stk_blks <- stack_blocks(all_stacks[[sid]])
      if (length(stk_blks) > 0L && all(stk_blks %in% block_ids)) {
        stk_ids <- c(stk_ids, sid)
      }
    }
    if (length(stk_ids) > 0L) {
      sub_stacks <- all_stacks[stk_ids]
    }
  }

  list(
    blocks = sub_blocks,
    links = sub_links,
    stacks = sub_stacks
  )
}

live_block_states <- function(board, block_ids) {
  running <- board$blocks
  ids <- intersect(block_ids, names(running))
  lapply(
    lst_xtr(running[ids], "server", "state"),
    lapply,
    reval_if
  )
}

blockr_ser_subgraph <- function(subgraph, block_states = NULL) {
  list(
    blocks = blockr_ser(subgraph$blocks, blocks = block_states),
    links = blockr_ser(subgraph$links),
    stacks = blockr_ser(subgraph$stacks)
  )
}

blockr_deser_subgraph <- function(data) {
  list(
    blocks = blockr_deser(data$blocks),
    links = blockr_deser(data$links),
    stacks = blockr_deser(data$stacks)
  )
}

remap_subgraph_ids <- function(subgraph, board) {
  existing_ids <- c(
    names(board_blocks(board)),
    names(board_links(board)),
    names(board_stacks(board))
  )

  old_block_ids <- names(subgraph$blocks)
  new_block_ids <- rand_names(old_names = existing_ids, n = length(old_block_ids))
  block_id_map <- set_names(new_block_ids, old_block_ids)

  # Remap blocks
  blk_list <- as.list(subgraph$blocks)
  names(blk_list) <- block_id_map[names(blk_list)]
  new_blocks <- as_blocks(blk_list)

  # Remap links
  new_links <- links()
  if (length(subgraph$links) > 0L) {
    link_list <- as.list(subgraph$links)
    remapped <- lapply(link_list, function(lnk) {
      new_link(
        from = unname(block_id_map[lnk$from]),
        to = unname(block_id_map[lnk$to]),
        input = lnk$input
      )
    })
    # Generate unique link IDs that don't collide with existing board IDs
    new_link_ids <- rand_names(
      old_names = c(existing_ids, new_block_ids),
      n = length(remapped)
    )
    names(remapped) <- new_link_ids
    new_links <- do.call(links, remapped)
  }

  # Remap stacks
  new_stacks <- stacks()
  if (length(subgraph$stacks) > 0L) {
    stk_list <- as.list(subgraph$stacks)
    remapped_stks <- lapply(stk_list, function(stk) {
      new_blk_ids <- unname(block_id_map[stack_blocks(stk)])
      new_name <- paste0(stack_name(stk), " (copy)")
      new_stack(blocks = new_blk_ids, name = new_name)
    })
    stk_new_ids <- rand_names(
      old_names = c(existing_ids, new_block_ids, names(new_links)),
      n = length(remapped_stks)
    )
    names(remapped_stks) <- stk_new_ids
    new_stacks <- as_stacks(remapped_stks)
  }

  list(
    blocks = new_blocks,
    links = new_links,
    stacks = new_stacks
  )
}
