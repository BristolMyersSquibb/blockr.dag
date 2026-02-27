blks_color <- function(blocks) {
  blockr.dock::blk_color(block_metadata(blocks)$category)
}

blks_icon <- function(blocks, size = 48L) {
  meta <- block_metadata(blocks)

  chr_mply(
    blockr.dock::blk_icon_data_uri,
    meta$icon,
    blockr.dock::blk_color(meta$category),
    MoreArgs = list(size = size)
  )
}
