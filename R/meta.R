blks_color <- function(blocks) {
  blockr.ui::category_color(block_metadata(blocks)$category)
}

# The node is blockr.ui's block mark at the dock header's size (32px), drawn
# as an image because the canvas cannot read the tokens.
blks_icon <- function(blocks) {
  meta <- block_metadata(blocks)

  chr_mply(
    blockr.ui::block_mark_svg,
    meta$icon,
    meta$category,
    MoreArgs = list(size = 32, uri = TRUE)
  )
}
