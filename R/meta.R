blk_category <- function(block) {

  id <- registry_id_from_block(block)

  if (length(id)) {
    block_metadata(id, "category")
  } else {
    default_category()
  }
}

blk_icon <- function(block) {

  id <- registry_id_from_block(block)

  if (length(id)) {
    block_metadata(id, "icon")
  } else {
    default_icon(default_category())
  }
}
