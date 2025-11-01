#' Block-specific icon mapping
#'
#' This file provides block-specific BS icons for the blockr ecosystem.
#' Icons are thin line variants (no "fill" icons) for a lighter appearance.
#'
#' In the future, block authors will add icon names to the registry.
#' For now, this mapping provides default icons for core blockr packages.
#'
#' @keywords internal

#' Get block-specific icon name
#'
#' Returns a BS icon name for a specific block. If no block-specific icon
#' is found, falls back to the category icon.
#'
#' @param block_id Character string, the block ID (e.g., "select", "mutate")
#' @param category Character string, the block category (for fallback)
#' @return Character string with BS icon name
#' @keywords internal
#' @examples
#' \dontrun{
#' blk_icon("select", "transform")  # Returns "funnel"
#' blk_icon("unknown", "transform") # Falls back to "shuffle" (category icon)
#' }
blk_icon <- function(block_id, category = NULL) {
  # Remove "new_" prefix and "_block" suffix to get clean block name
  clean_id <- sub("^new_", "", block_id)
  clean_id <- sub("_block$", "", clean_id)

  # Get block-specific icon mapping
  icon_map <- blk_icon_mapping()

  # Return block-specific icon if available
  if (clean_id %in% names(icon_map)) {
    return(icon_map[[clean_id]])
  }

  # Fallback to category icon if category provided
  if (!is.null(category)) {
    return(blk_icon_name(category))
  }

  # Final fallback
  "question-circle"
}

#' Block-to-icon mapping table
#'
#' Maps block names to BS icon names for core blockr packages
#' (blockr.dplyr, blockr.ggplot, blockr.io).
#'
#' All icons are thin line variants (no "-fill" suffix) for consistent
#' lightweight appearance.
#'
#' @return Named character vector mapping block names to icon names
#' @keywords internal
blk_icon_mapping <- function() {
  c(
    # blockr.dplyr blocks (transform category)
    "select" = "check-square",           # Select/subset columns
    "join" = "link-45deg",               # Join tables together
    "arrange" = "sort-down",             # Sort/order rows
    "mutate" = "pencil-square",          # Modify/add columns
    "summarize" = "calculator",          # Aggregate/summarize
    "filter_expr" = "funnel",            # Filter by expression
    "value_filter" = "sliders",          # Filter by value selection
    "bind_rows" = "arrows-collapse-vertical", # Stack rows vertically
    "bind_cols" = "arrows-collapse",     # Combine columns horizontally
    "rename" = "input-cursor-text",      # Rename columns
    "slice" = "scissors",                # Slice/select rows by position
    "pivot_longer" = "arrow-down-up",    # Reshape wide to long
    "pivot_wider" = "arrow-left-right",  # Reshape long to wide

    # blockr.ggplot blocks (plot category)
    "ggplot" = "bar-chart-line",         # Main ggplot visualization
    "grid" = "grid-3x3",                 # Grid/patchwork layout
    "theme" = "palette2",                # Theme customization
    "facet" = "grid-3x2",                # Facet/panel splitting

    # blockr.io blocks (input category)
    "read" = "file-earmark-arrow-up"     # Read data from files
  )
}
