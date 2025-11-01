#' Generate BS icon SVG string with fallback
#'
#' Generates an SVG string for a BS icon with graceful fallback to category
#' icon and then default icon if the requested icon doesn't exist.
#'
#' @param icon_name Character, the BS icon name to generate
#' @param category Character, category for fallback icon
#' @return Character, SVG string
#' @keywords internal
blk_icon_svg <- function(icon_name, category = NULL) {
  tryCatch(
    as.character(bsicons::bs_icon(icon_name)),
    error = function(e) {
      # Fallback to category icon
      if (!is.null(category)) {
        category_icon <- blk_icon_name(category)
        tryCatch(
          as.character(bsicons::bs_icon(category_icon)),
          error = function(e2) {
            # Final fallback to default icon
            as.character(bsicons::bs_icon("question-circle"))
          }
        )
      } else {
        # No category, use default
        as.character(bsicons::bs_icon("question-circle"))
      }
    }
  )
}

#' Shared CSS for block selectize components
#'
#' Provides standardized CSS styling for selectize components used in
#' block selection interfaces (both registry and board blocks).
#'
#' Defines the following classes:
#' - `.selectize-dropdown-content` - Dropdown content area styling
#' - `.block-option` - Option card layout with padding and spacing
#' - `.block-icon-wrapper` - Icon container (40px) with flex centering
#' - `.block-content` - Flexible content area for text
#' - `.block-header` - Header flex layout for title and badge
#' - `.block-name` - Block name styling (bold, dark text)
#' - `.block-desc` - Description text styling (smaller, gray)
#' - `.badge-two-tone` - Package badge styling (subtle background)
#'
#' @return HTML style tag with block selectize CSS
#' @keywords internal
css_block_selectize <- function() {
  tags$style(HTML(
    "
    .selectize-dropdown-content {
      max-height: 450px !important;
      padding: 8px 0;
    }
    .block-option {
      padding: 16px 24px;
      display: flex;
      align-items: flex-start;
      gap: 16px;
      margin: 4px 8px;
      border-radius: 6px;
      transition: background-color 0.15s ease;
    }
    .block-icon-wrapper {
      flex-shrink: 0;
      width: 40px;
      height: 40px;
      border-radius: 8px;
      display: flex;
      align-items: center;
      justify-content: center;
      font-size: 18px;
      color: white;
    }
    .block-content {
      flex: 1;
      min-width: 0;
    }
    .block-header {
      display: flex;
      align-items: flex-start;
      justify-content: space-between;
      gap: 10px;
      margin-bottom: 4px;
    }
    .block-name {
      font-weight: 600;
      font-size: 15px;
      color: #212529;
      flex: 1;
    }
    .block-desc {
      font-size: 13px;
      color: #6c757d;
      line-height: 1.4;
    }
    .badge-two-tone {
      display: inline-block;
      padding: 0.125rem 0.375rem;
      font-size: 0.625rem;
      border-radius: 0.25rem;
      background-color: rgba(148, 163, 184, 0.1);
      color: rgba(100, 116, 139, 0.9);
      border: 1px solid rgba(100, 116, 139, 0.1);
      white-space: nowrap;
      flex-shrink: 0;
    }
    .selectize-dropdown .block-option {
      border-bottom: 1px solid #f0f0f0;
    }
    .selectize-dropdown .block-option:last-child {
      border-bottom: none;
    }
    .selectize-dropdown .block-option:hover,
    .selectize-dropdown .block-option.active {
      background-color: #e9ecef;
    }
    "
  ))
}

create_block_modal <- function(
  mode = c("append", "add"),
  ns,
  board_block_ids,
  board_link_ids = NULL
) {
  mode <- match.arg(mode)

  title <- if (mode == "append") "Append new block" else "Add new block"
  button_label <- if (mode == "append") "Append Block" else "Add Block"

  # Use different IDs for each mode to avoid conflicts
  selection_id <- paste0(mode, "_block_selection")
  name_id <- paste0(mode, "_block_name")
  block_id_field <- paste0(mode, "_block_id")
  confirm_id <- paste0(mode, "_block_confirm")

  # CSS for advanced options toggle and compact modal styling
  advanced_css <- tags$style(HTML(sprintf(
    "
    #%s {
      max-height: 0;
      overflow: hidden;
      transition: max-height 0.3s ease-out;
    }
    #%s.expanded {
      max-height: 500px;
      overflow: visible;
      transition: max-height 0.5s ease-in;
    }
    .block-advanced-toggle {
      cursor: pointer;
      user-select: none;
      padding: 8px 0;
      margin-bottom: 8px;
      display: flex;
      align-items: center;
      gap: 6px;
      color: #6c757d;
      font-size: 0.875rem;
    }
    .block-chevron {
      transition: transform 0.2s;
      display: inline-block;
      font-size: 14px;
      font-weight: bold;
    }
    .block-chevron.rotated {
      transform: rotate(90deg);
    }
    /* Compact modal styling */
    #shiny-modal .modal-header {
      padding: 12px 20px;
      border-bottom: 1px solid #e2e8f0;
    }
    #shiny-modal .modal-title {
      font-size: 1.125rem;
      font-weight: 600;
      margin: 0;
    }
    #shiny-modal .modal-body {
      padding: 20px;
    }
    #shiny-modal .modal-body .form-group {
      width: 100%%;
      margin-bottom: 16px;
    }
    #shiny-modal .modal-body .selectize-input,
    #shiny-modal .modal-body input[type='text'] {
      width: 100%%;
    }
    #shiny-modal .modal-body .shiny-input-container {
      width: 100%%;
    }
    #shiny-modal .modal-body .control-label {
      font-size: 0.875rem;
      color: #6c757d;
      margin-bottom: 4px;
      font-weight: normal;
    }
    #shiny-modal .modal-footer {
      padding: 12px 20px;
      border-top: 1px solid #e2e8f0;
      gap: 8px;
    }
    #shiny-modal .modal-footer .btn {
      font-size: 0.875rem;
      padding: 0.375rem 0.75rem;
    }
  ",
    ns("advanced-options"),
    ns("advanced-options")
  )))

  # Always visible fields
  visible_fields <- list(
    block_registry_selectize(ns(selection_id))
  )

  # Add block name field (visible)
  visible_fields[[length(visible_fields) + 1]] <- textInput(
    ns(name_id),
    label = "User defined block title (can be changed after creation)",
    placeholder = "Select block first"
  )

  # Advanced options toggle button
  toggle_button <- div(
    class = "block-advanced-toggle text-muted",
    id = ns("advanced-toggle"),
    onclick = sprintf(
      "
      const section = document.getElementById('%s');
      const chevron = document.querySelector('#%s .block-chevron');
      section.classList.toggle('expanded');
      chevron.classList.toggle('rotated');
      ",
      ns("advanced-options"),
      ns("advanced-toggle")
    ),
    tags$span(class = "block-chevron", "\u203A"),
    "Show advanced options"
  )

  # Advanced options (collapsible)
  advanced_fields <- list()

  # Add block input field only for append mode (in advanced options)
  if (mode == "append") {
    advanced_fields[[length(advanced_fields) + 1]] <- selectizeInput(
      ns("append_block_input"),
      "Block input",
      choices = c(`Select a block to populate options` = "")
    )
  }

  # Add Block ID field
  advanced_fields[[length(advanced_fields) + 1]] <- textInput(
    ns(block_id_field),
    label = "Block ID",
    value = rand_names(board_block_ids)
  )

  # Add link ID field only for append mode (in advanced options)
  if (mode == "append") {
    advanced_fields[[length(advanced_fields) + 1]] <- textInput(
      ns("append_link_id"),
      label = "Link ID",
      value = rand_names(board_link_ids)
    )
  }

  # Collapsible advanced options section
  advanced_section <- div(
    id = ns("advanced-options"),
    tagList(advanced_fields)
  )

  # Button section (right-aligned at bottom)
  button_section <- div(
    style = "display: flex; justify-content: flex-end; margin-top: 20px;",
    actionButton(
      ns(confirm_id),
      button_label,
      class = "btn-primary"
    )
  )

  # Add auto-focus script
  auto_focus_script <- tags$script(HTML(sprintf(
    "
    $('#shiny-modal').on('shown.bs.modal', function() {
      $('#%s')[0].selectize.focus();
    });
  ",
    ns(selection_id)
  )))

  modalDialog(
    title = title,
    size = "l",
    easyClose = TRUE,
    footer = NULL,
    tagList(
      advanced_css,
      visible_fields,
      toggle_button,
      advanced_section,
      button_section,
      auto_focus_script
    )
  )
}

block_registry_selectize <- function(id, blocks = list_blocks()) {

  options_data <- apply(
    block_metadata(blocks),
    1L,
    function(blk) {
      list(
        value = blk$id,
        label = blk$name,
        description = blk$description,
        category = blk$category,
        package = blk$package,
        icon = blk$icon,
        color = blk_color(blk$category),
        searchtext = paste(blk$name, blk$description, blk$package)
      )
    }
  )

  tagList(
    css_block_selectize(),
    tags$style(HTML(
      "
      /* Hide the dropdown arrow for block registry selectize */
      .block-registry-selectize .selectize-input:after {
        display: none !important;
      }
    "
    )),
    tags$div(
      class = "block-registry-selectize",
      selectizeInput(
        id,
        label = "Select block to add",
        choices = NULL,
        options = list(
          options = options_data,
          valueField = "value",
          labelField = "label",
          searchField = c("label", "description", "searchtext"),
          placeholder = "Type to search blocks...",
          openOnFocus = FALSE,
          render = I(
            "{
          item: function(item, escape) {
            // Comfortable card-style layout for selected item
            var name = escape(item.label);
            var desc = escape(item.description || '');
            var pkg = escape(item.package || '');
            var color = item.color || '#6c757d';

            // Get SVG and style it (larger size for comfortable layout)
            var iconSvg = item.icon_svg || '';
            var styledSvg = iconSvg.replace('<svg', '<svg style=\"width: 20px; height: 20px; fill: white;\"');

            // Icon wrapper (40px for comfortable size)
            var iconWrapper = '<div class=\"block-icon-wrapper\" style=\"background-color: ' + color + '; width: 40px; height: 40px; border-radius: 8px; display: flex; align-items: center; justify-content: center; flex-shrink: 0;\">' +
                              styledSvg + '</div>';

            // Package badge
            var pkgBadge = pkg ?
                           '<div class=\"badge-two-tone\">' + pkg + '</div>' :
                           '';

            // Description
            var descHtml = desc ?
                           '<div class=\"block-desc\">' + desc + '</div>' :
                           '';

            // Card layout: icon on left, content (title, desc) stacked on right, badge at top right
            return '<div style=\"display: flex; align-items: flex-start; gap: 16px; padding: 12px; background-color: #f8f9fa; border-radius: 8px; border: 1px solid #e9ecef;\">' +
                   iconWrapper +
                   '<div style=\"flex: 1; min-width: 0;\">' +
                   '<div style=\"display: flex; align-items: flex-start; justify-content: space-between; gap: 10px; margin-bottom: 4px;\">' +
                   '<div class=\"block-name\">' + name + '</div>' +
                   pkgBadge +
                   '</div>' +
                   descHtml +
                   '</div>' +
                   '</div>';
          },
          option: function(item, escape) {
            var name = escape(item.label);
            var desc = escape(item.description || '');
            var pkg = escape(item.package || '');
            var color = item.color || '#6c757d';

            // Get SVG and style it (larger size for comfortable layout)
            var iconSvg = item.icon_svg || '';
            var styledSvg = iconSvg.replace(
              '<svg',
              '<svg style=\"width: 20px; height: 20px; fill: white;\"'
            );

            var iconWrapper = '<div class=\"block-icon-wrapper\" ' +
                              'style=\"background-color: ' + color + ';\">' +
                              styledSvg + '</div>';
            var pkgBadge = pkg ?
                           '<div class=\"badge-two-tone\">' + pkg + '</div>' :
                           '';
            var descHtml = desc ?
                           '<div class=\"block-desc\">' + desc + '</div>' :
                           '';
            return '<div class=\"block-option\">' + iconWrapper +
                     '<div class=\"block-content\">' +
                       '<div class=\"block-header\">' +
                         '<div class=\"block-name\">' + name + '</div>' +
                         pkgBadge +
                       '</div>' +
                       descHtml +
                     '</div>' +
                   '</div>';
          }
        }"
          )
        )
      )
    )
  )
}

stack_modal <- function(
  ns,
  board_block_ids,
  board_blocks,
  mode = c("create", "edit"),
  stack = NULL,
  stack_id = NULL,
  board_stack_ids = NULL
) {
  mode <- match.arg(mode)

  # Mode-specific values
  title <- if (mode == "create") "Create new stack" else "Edit stack"
  button_label <- if (mode == "create") "Create Stack" else "Update Stack"

  selection_id <- if (mode == "create") "stack_block_selection" else "edit_stack_blocks"
  name_id <- if (mode == "create") "stack_name" else "edit_stack_name"
  stack_id_field <- "stack_id"
  confirm_id <- if (mode == "create") "stack_confirm" else "edit_stack_confirm"

  # CSS for advanced options toggle and compact modal styling
  advanced_css <- tags$style(HTML(sprintf(
    "
    #%s {
      max-height: 0;
      overflow: hidden;
      transition: max-height 0.3s ease-out;
    }
    #%s.expanded {
      max-height: 500px;
      overflow: visible;
      transition: max-height 0.5s ease-in;
    }
    .stack-advanced-toggle {
      cursor: pointer;
      user-select: none;
      padding: 8px 0;
      margin-bottom: 8px;
      display: flex;
      align-items: center;
      gap: 6px;
      color: #6c757d;
      font-size: 0.875rem;
    }
    .stack-chevron {
      transition: transform 0.2s;
      display: inline-block;
      font-size: 14px;
      font-weight: bold;
    }
    .stack-chevron.rotated {
      transform: rotate(90deg);
    }
    /* Compact modal styling */
    #shiny-modal .modal-header {
      padding: 12px 20px;
      border-bottom: 1px solid #e2e8f0;
    }
    #shiny-modal .modal-title {
      font-size: 1.125rem;
      font-weight: 600;
      margin: 0;
    }
    #shiny-modal .modal-body {
      padding: 20px;
    }
    #shiny-modal .modal-body .form-group {
      width: 100%%;
      margin-bottom: 16px;
    }
    #shiny-modal .modal-body .selectize-input,
    #shiny-modal .modal-body input[type='text'] {
      width: 100%%;
    }
    #shiny-modal .modal-body .shiny-input-container {
      width: 100%%;
    }
    #shiny-modal .modal-body .control-label {
      font-size: 0.875rem;
      color: #6c757d;
      margin-bottom: 4px;
      font-weight: normal;
    }
    #shiny-modal .modal-footer {
      padding: 12px 20px;
      border-top: 1px solid #e2e8f0;
      gap: 8px;
    }
    #shiny-modal .modal-footer .btn {
      font-size: 0.875rem;
      padding: 0.375rem 0.75rem;
    }
  ",
    ns("stack-advanced-options"),
    ns("stack-advanced-options")
  )))

  # Always visible fields
  visible_fields <- list(
    board_blocks_selectize(
      id = ns(selection_id),
      board_blocks = board_blocks,
      board_block_ids = board_block_ids,
      selected = if (mode == "edit") stack_blocks(stack) else NULL
    )
  )

  # Add stack name field (visible)
  visible_fields[[length(visible_fields) + 1]] <- textInput(
    ns(name_id),
    label = if (mode == "create") "Stack name (can be changed after creation)" else "Stack name",
    placeholder = if (mode == "create") "Enter stack name" else NULL,
    value = if (mode == "edit") stack_name(stack) else NULL
  )

  # For edit mode, add color picker to visible fields
  if (mode == "edit") {
    visible_fields[[length(visible_fields) + 1]] <- shinyWidgets::colorPickr(
      inputId = ns("edit_stack_color"),
      label = "Stack color",
      selected = stack_color(stack),
      theme = "nano",
      position = "right-end",
      useAsButton = TRUE
    )
  }

  # Advanced options (only for create mode)
  toggle_button <- NULL
  advanced_section <- NULL

  if (mode == "create") {
    # Advanced options toggle button
    toggle_button <- div(
      class = "stack-advanced-toggle text-muted",
      id = ns("stack-advanced-toggle"),
      onclick = sprintf(
        "
        const section = document.getElementById('%s');
        const chevron = document.querySelector('#%s .stack-chevron');
        section.classList.toggle('expanded');
        chevron.classList.toggle('rotated');
        ",
        ns("stack-advanced-options"),
        ns("stack-advanced-toggle")
      ),
      tags$span(class = "stack-chevron", "\u203A"),
      "Show advanced options"
    )

    # Advanced options (collapsible)
    advanced_fields <- list()

    # Add color picker field
    advanced_fields[[length(advanced_fields) + 1]] <- shinyWidgets::colorPickr(
      inputId = ns("stack_color"),
      label = "Stack color",
      selected = suggest_new_colors(
        chr_ply(board_blocks, function(b) {
          meta <- get_block_metadata(b)
          blk_color(meta$category)
        })
      ),
      theme = "nano",
      position = "right-end",
      useAsButton = TRUE
    )

    # Add Stack ID field
    advanced_fields[[length(advanced_fields) + 1]] <- textInput(
      ns(stack_id_field),
      label = "Stack ID",
      value = rand_names(board_stack_ids)
    )

    # Collapsible advanced options section
    advanced_section <- div(
      id = ns("stack-advanced-options"),
      tagList(advanced_fields)
    )
  }

  # Button section (right-aligned at bottom)
  button_section <- div(
    style = "display: flex; justify-content: flex-end; margin-top: 20px;",
    actionButton(
      ns(confirm_id),
      button_label,
      class = "btn-primary"
    )
  )

  # Add auto-focus script
  auto_focus_script <- tags$script(HTML(sprintf(
    "
    $('#shiny-modal').on('shown.bs.modal', function() {
      $('#%s')[0].selectize.focus();
    });
  ",
    ns(selection_id)
  )))

  modalDialog(
    title = title,
    size = "l",
    easyClose = TRUE,
    footer = NULL,
    tagList(
      advanced_css,
      visible_fields,
      toggle_button,
      advanced_section,
      button_section,
      auto_focus_script
    )
  )
}

board_blocks_selectize <- function(
  id,
  board_blocks,
  board_block_ids,
  selected = NULL
) {
  # Build options list for selectize from board blocks
  options_data <- list()
  for (block_id in board_block_ids) {
    block <- board_blocks[[block_id]]

    # Check if it's a simple data structure (list with id/name/package) or a block object
    if (is.list(block) && !is_block(block) && "id" %in% names(block)) {
      # Simple data structure for UI testing
      name <- block$name
      if (is.null(name) || nchar(name) == 0) {
        name <- block_id
      }
      pkg <- block$package
      if (is.null(pkg)) {
        pkg <- ""
      }
      category <- block$category
      if (is.null(category)) {
        category <- "uncategorized"
      }
      # Use icon from block data if available, otherwise use default
      icon <- block$icon
      if (is.null(icon)) {
        icon <- "question-square"
      }
      color <- block$color
      if (is.null(color)) {
        color <- blk_color(category)
      }
    } else {
      # Actual block object
      name <- block_name(block)
      if (is.null(name) || nchar(name) == 0) {
        name <- block_id
      }

      # Get block metadata including package, category, icon, and color
      metadata <- get_block_metadata(block)
      pkg <- metadata$package
      if (is.null(pkg)) {
        pkg <- ""
      }
      category <- metadata$category
      if (is.null(category)) {
        category <- "uncategorized"
      }
      icon <- metadata$icon
      if (is.null(icon)) {
        icon <- "question-square"
      }
      color <- blk_color(category)
    }

    # Generate SVG string for the icon using bsicons
    icon_svg <- as.character(bsicons::bs_icon(icon))

    options_data[[length(options_data) + 1]] <- list(
      value = block_id,
      label = name,
      description = sprintf("ID: %s", block_id),
      block_id = block_id,
      block_name = name,
      package = pkg,
      category = category,
      icon = icon,
      icon_svg = icon_svg,
      color = color,
      searchtext = paste(name, block_id, pkg)
    )
  }

  tagList(
    css_block_selectize(),
    tags$style(HTML(
      "
      /* Style the remove button */
      .selectize-input .remove {
        text-decoration: none !important;
        color: #6c757d !important;
        font-weight: normal !important;
        border: none !important;
        margin-left: 8px !important;
        padding: 2px 6px !important;
        border-radius: 3px !important;
        transition: background-color 0.2s ease, color 0.2s ease;
      }
      .selectize-input .remove:hover {
        background-color: rgba(108, 117, 125, 0.1) !important;
        color: #495057 !important;
      }
    "
    )),
    selectizeInput(
      id,
      label = "Select blocks to add to stack (optional)",
      choices = NULL,
      selected = selected,
      multiple = TRUE,
      options = {
        opts <- list(
          options = options_data,
          valueField = "value",
          labelField = "label",
          searchField = c("label", "description", "searchtext"),
          placeholder = "Type to search blocks...",
          openOnFocus = FALSE,
          plugins = list("remove_button", "drag_drop"),
          render = I(
            "{
          item: function(item, escape) {
            // item is the full option object when rendering selected items
            var name = escape(item.label || item.block_name || '');
            var blockId = escape(item.block_id || '');
            var pkg = escape(item.package || '');
            var icon = item.icon || 'box';
            var color = item.color || '#6c757d';

            // Build icon wrapper with gray background padding and inline SVG
            var iconSvg = item.icon_svg || '';
            // Style the SVG: set width, height, and color
            var styledSvg = iconSvg.replace('<svg', '<svg style=\"width: 14px; height: 14px; fill: white;\"');

            var iconWrapper = '<span style=\"background-color: #f8f9fa; display: inline-flex; align-items: center; justify-content: center; padding: 6px; border-radius: 8px; margin-right: 10px;\">' +
                              '<span style=\"background-color: ' + color + '; display: inline-flex; align-items: center; justify-content: center; width: 28px; height: 28px; border-radius: 6px;\">' +
                              styledSvg + '</span>' +
                              '</span>';

            // Title with type and ID next to it
            var titleWithId = '<div style=\"display: flex; align-items: center; flex: 1;\">' +
                             '<span style=\"font-weight: bold; font-size: 0.9em; color: #212529;\">' + name + '</span>' +
                             (blockId ? '<span style=\"font-size: 0.75em; color: #6c757d; margin-left: 6px;\">type: ' + name + ' &middot; ID: ' + blockId + '</span>' : '') +
                             '</div>';

            // Build package badge (positioned to the right)
            var pkgBadge = pkg ?
                           '<span style=\"display: inline-block; padding: 0.125rem 0.375rem; font-size: 0.625rem; border-radius: 0.25rem; background-color: rgba(148, 163, 184, 0.1); color: rgba(100, 116, 139, 0.9); border: 1px solid rgba(100, 116, 139, 0.1); white-space: nowrap; margin-right: 8px;\">' + pkg + '</span>' :
                           '';

            return '<div style=\"display: flex; align-items: center; width: 100%; padding: 4px 8px; background-color: #f8f9fa; border-radius: 8px; border: 1px solid #e9ecef; margin: 4px 0;\">' +
                   iconWrapper +
                   titleWithId +
                   pkgBadge +
                   '</div>';
          },
          option: function(item, escape) {
            var name = escape(item.block_name || '');
            var blockId = escape(item.block_id || '');
            var pkg = escape(item.package || '');
            var icon = item.icon || 'box';
            var color = item.color || '#6c757d';

            // Same icon wrapper as item render (but without outer padding wrapper)
            var iconSvg = item.icon_svg || '';
            // Style the SVG: set width, height, and color
            var styledSvg = iconSvg.replace('<svg', '<svg style=\"width: 14px; height: 14px; fill: white;\"');

            var iconWrapper = '<span style=\"background-color: ' + color + '; display: inline-flex; align-items: center; justify-content: center; width: 28px; height: 28px; border-radius: 6px; margin-right: 8px; padding: 4px;\">' +
                              styledSvg + '</span>';

            // Title with type and ID next to it (same as item)
            var titleWithId = '<div style=\"display: flex; align-items: center; flex: 1;\">' +
                             '<span style=\"font-weight: bold; font-size: 0.9em; color: #212529;\">' + name + '</span>' +
                             (blockId ? '<span style=\"font-size: 0.75em; color: #6c757d; margin-left: 6px;\">type: ' + name + ' &middot; ID: ' + blockId + '</span>' : '') +
                             '</div>';

            // Package badge (same as item)
            var pkgBadge = pkg ?
                           '<span style=\"display: inline-block; padding: 0.125rem 0.375rem; font-size: 0.625rem; border-radius: 0.25rem; background-color: rgba(148, 163, 184, 0.1); color: rgba(100, 116, 139, 0.9); border: 1px solid rgba(100, 116, 139, 0.1); white-space: nowrap; margin-left: 6px;\">' + escape(pkg) + '</span>' :
                           '';

            // Same container style as item (but no remove button) - with margin for spacing
            return '<div style=\"display: flex; align-items: center; justify-content: space-between; padding: 10px 16px; background-color: #f8f9fa; border-radius: 8px; border: 1px solid #e9ecef; margin: 4px 12px;\">' +
                   iconWrapper +
                   titleWithId +
                   pkgBadge +
                   '</div>';
          }
        }"
          )
        )
        if (!is.null(selected) && length(selected) > 0) {
          # Preselect items - items should be an array of values
          opts$items <- as.list(selected)
        }
        opts
      }
    ),
    if (!is.null(selected) && length(selected) > 0) {
      tags$script(HTML(sprintf(
        "
        $(document).ready(function() {
          function setSelectedValues() {
            var $select = $('#%s');
            if ($select.length) {
              var selectize = $select[0].selectize;
              if (selectize) {
                try {
                  selectize.setValue(%s, true);
                  return true;
                } catch(e) {
                  console.log('Error setting values:', e);
                  return false;
                }
              }
            }
            return false;
          }
          // Try multiple times with increasing delays
          var attempts = 0;
          var maxAttempts = 10;
          var interval = setInterval(function() {
            attempts++;
            if (setSelectedValues() || attempts >= maxAttempts) {
              clearInterval(interval);
            }
          }, 300);
        });
        ",
        id,
        paste0("['", paste(selected, collapse = "','"), "']")
      )))
    }
  )
}
