#' Shared CSS for modal advanced options and compact styling
#'
#' @param advanced_id The namespaced ID for the advanced options container
#' @return HTML style tag with modal CSS
#' @rdname modal
#' @keywords internal
css_modal_advanced <- function(advanced_id) {
  tags$style(HTML(sprintf(
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
    .modal-advanced-toggle {
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
    .modal-chevron {
      transition: transform 0.2s;
      display: inline-block;
      font-size: 14px;
      font-weight: bold;
    }
    .modal-chevron.rotated {
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
    advanced_id,
    advanced_id
  )))
}

#' @rdname modal
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

#' @rdname modal
#' @keywords internal
js_blk_selectize_render <- function() {
  I(
    "{
      item: function(item, escape) {
        var name = escape(item.label);
        var pkg = escape(item.package || '');
        var color = item.color || '#6c757d';

        var iconSvg = item.icon || '';
        var styledSvg = iconSvg.replace(
          '<svg ',
          '<svg style=\"width: 14px; height: 14px; fill: white;\" '
        );

        var containerStyle =
          'display: inline-flex; align-items: center; gap: 8px; ' +
          'padding: 4px 8px; background-color: #f8f9fa; ' +
          'border-radius: 6px; border: 1px solid #e9ecef;';
        var iconStyle =
          'background-color: ' + color + '; width: 24px; ' +
          'height: 24px; border-radius: 4px; display: flex; ' +
          'align-items: center; justify-content: center; flex-shrink: 0;';
        var pkgBadgeHtml = pkg ?
          '<div class=\"badge-two-tone\" style=\"margin-left: 4px;\">' +
          pkg + '</div>' : '';
        return '<div style=\"' + containerStyle + '\">' +
               '<div style=\"' + iconStyle + '\">' + styledSvg + '</div>' +
               '<div style=\"font-weight: 500; font-size: 14px;\">' +
               name + '</div>' + pkgBadgeHtml + '</div>';
      },
      option: function(item, escape) {
        var name = escape(item.label);
        var desc = escape(item.description || '');
        var pkg = escape(item.package || '');
        var color = item.color || '#6c757d';

        var iconSvg = item.icon || '';
        var styledSvg = iconSvg.replace(
          '<svg ',
          '<svg style=\"width: 20px; height: 20px; fill: white;\" '
        );

        var iconWrapperStyle = 'background-color: ' + color + ';';
        var iconWrapper = '<div class=\"block-icon-wrapper\" ' +
                          'style=\"' + iconWrapperStyle + '\">' +
                          styledSvg + '</div>';

        var pkgBadge = pkg ?
                       '<div class=\"badge-two-tone\">' + pkg +
                       '</div>' : '';

        // For board blocks, show type/ID info as description
        // For registry blocks, show the description field
        var descHtml = '';
        if (item.block_type) {
          var blockType = escape(item.block_type);
          var blockId = escape(item.block_id || '');
          descHtml = '<div class=\"block-desc\">type: ' + blockType +
                     (blockId ? ' &middot; ID: ' + blockId : '') + '</div>';
        } else if (desc) {
          descHtml = '<div class=\"block-desc\">' + desc + '</div>';
        }

        return '<div class=\"block-option\">' + iconWrapper +
                 '<div class=\"block-content\">' +
                   '<div class=\"block-header\">' +
                     '<div class=\"block-name\">' + name +
                     '</div>' + pkgBadge + '</div>' +
                   descHtml + '</div>' + '</div>';
      }
    }"
  )
}

create_block_modal <- function(mode = c("append", "add"), ns, board) {
  mode <- match.arg(mode)

  board_block_ids <- board_block_ids(board)
  board_link_ids <- board_link_ids(board)

  title <- if (mode == "append") "Append new block" else "Add new block"
  button_label <- if (mode == "append") "Append Block" else "Add Block"

  # Use different IDs for each mode to avoid conflicts
  selection_id <- paste0(mode, "_block_selection")
  name_id <- paste0(mode, "_block_name")
  block_id_field <- paste0(mode, "_block_id")
  confirm_id <- paste0(mode, "_block_confirm")

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
    class = "modal-advanced-toggle text-muted",
    id = ns("block-advanced-toggle"),
    onclick = sprintf(
      "
      const section = document.getElementById('%s');
      const chevron = document.querySelector('#%s .modal-chevron');
      section.classList.toggle('expanded');
      chevron.classList.toggle('rotated');
      ",
      ns("block-advanced-options"),
      ns("block-advanced-toggle")
    ),
    tags$span(class = "modal-chevron", "\u203A"),
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
    id = ns("block-advanced-options"),
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
  auto_focus_script <- tags$script(HTML(sprintf("
    $('#shiny-modal').on('shown.bs.modal', function() {
      $('#%s')[0].selectize.focus();
    });
  ", ns(selection_id))))

  modalDialog(
    title = title,
    size = "l",
    easyClose = TRUE,
    footer = NULL,
    tagList(
      css_modal_advanced(ns("block-advanced-options")),
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
        value = unname(blk["id"]),
        label = unname(blk["name"]),
        description = unname(blk["description"]),
        category = unname(blk["category"]),
        package = unname(blk["package"]),
        icon = unname(blk["icon"]),
        color = blk_color(blk["category"]),
        searchtext = paste(blk["name"], blk["description"], blk["package"])
      )
    }
  )

  tagList(
    css_block_selectize(),
    selectizeInput(
      id,
      label = "Select block to add",
      choices = NULL,
      options = list(
        options = options_data,
        valueField = "value",
        labelField = "label",
        searchField = c("label", "description", "searchtext"),
        placeholder = "Type to search",
        openOnFocus = FALSE,
        render = js_blk_selectize_render()
      )
    )
  )
}

link_modal <- function(ns, board, block_id) {

  board_blocks <- board_blocks(board)

  stopifnot(is_string(block_id), block_id %in% names(board_blocks))

  invisible()
}

stack_modal <- function(ns, board, mode = c("create", "edit"), stack = NULL,
                        stack_id = NULL) {

  mode <- match.arg(mode)

  board_blocks <- board_blocks(board)
  board_stack_ids <- board_stack_ids(board)
  board_stacks <- board_stacks(board)

  avail <- available_stack_blocks(board)

  if (mode == "edit") {
    sel <- stack_blocks(stack)
    avail <- c(avail, sel)
  } else {
    sel <- NULL
  }

  board_blocks <- board_blocks[avail]

  # Mode-specific values
  title <- if (mode == "create") "Create new stack" else "Edit stack"
  button_label <- if (mode == "create") "Create Stack" else "Update Stack"

  selection_id <- if (mode == "create") {
    "stack_block_selection"
  } else {
    "edit_stack_blocks"
  }
  name_id <- if (mode == "create") "stack_name" else "edit_stack_name"
  stack_id_field <- "stack_id"
  confirm_id <- if (mode == "create") "stack_confirm" else "edit_stack_confirm"

  # Always visible fields
  visible_fields <- list(
    board_select(
      id = ns(selection_id),
      blocks = board_blocks,
      selected = sel
    )
  )

  # Add stack name field (visible)
  visible_fields[[length(visible_fields) + 1]] <- textInput(
    ns(name_id),
    label = if (mode == "create") {
      "Stack name (can be changed after creation)"
    } else {
      "Stack name"
    },
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
      class = "modal-advanced-toggle text-muted",
      id = ns("stack-advanced-toggle"),
      onclick = sprintf(
        "
        const section = document.getElementById('%s');
        const chevron = document.querySelector('#%s .modal-chevron');
        section.classList.toggle('expanded');
        chevron.classList.toggle('rotated');
        ",
        ns("stack-advanced-options"),
        ns("stack-advanced-toggle")
      ),
      tags$span(class = "modal-chevron", "\u203A"),
      "Show advanced options"
    )

    # Advanced options (collapsible)
    advanced_fields <- list()

    # Add color picker field
    advanced_fields[[length(advanced_fields) + 1]] <- shinyWidgets::colorPickr(
      inputId = ns("stack_color"),
      label = "Stack color",
      selected = suggest_new_colors(
        stack_color(board_stacks)
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
      css_modal_advanced(ns("stack-advanced-options")),
      visible_fields,
      toggle_button,
      advanced_section,
      button_section,
      auto_focus_script
    )
  )
}

board_select <- function(id, blocks, selected = NULL) {

  meta <- blks_metadata(blocks)

  bid <- names(blocks)
  bnm <- chr_ply(blocks, block_name)

  options_data <- map(
    list,
    value = bid,
    label = bnm,
    block_id = bid,
    block_name = bnm,
    block_type = meta$name,
    package = meta$package,
    category = meta$category,
    icon = meta$icon,
    color = meta$color,
    searchtext = paste(bnm, bid, meta$package)
  )

  options <- list(
    options = options_data,
    valueField = "value",
    labelField = "label",
    searchField = c("label", "description", "searchtext"),
    placeholder = "Type to search blocks...",
    openOnFocus = FALSE,
    plugins = list("remove_button", "drag_drop"),
    render = js_blk_selectize_render()
  )

  if (!is.null(selected) && length(selected) > 0) {
    # Preselect items - items should be an array of values
    options$items <- as.list(selected)
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
      options = options
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
