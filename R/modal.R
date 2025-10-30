create_block_modal <- function(mode = c("append", "add"), ns,
                               board_block_ids, board_link_ids = NULL) {
  mode <- match.arg(mode)

  title <- if (mode == "append") "Append new block" else "Add new block"
  button_label <- if (mode == "append") "Append Block" else "Add Block"

  # Use different IDs for each mode to avoid conflicts
  selection_id <- paste0(mode, "_block_selection")
  name_id <- paste0(mode, "_block_name")
  block_id_field <- paste0(mode, "_block_id")
  confirm_id <- paste0(mode, "_block_confirm")

  # CSS for advanced options toggle and compact modal styling
  advanced_css <- tags$style(HTML(sprintf("
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
  ", ns("advanced-options"), ns("advanced-options"))))

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
      advanced_css,
      visible_fields,
      toggle_button,
      advanced_section,
      button_section,
      auto_focus_script
    )
  )
}

block_registry_selectize <- function(id) {
  # Get all available blocks with metadata
  blocks <- available_blocks()
  block_ids <- list_blocks()

  # Build options list for selectize
  options_data <- list()
  for (uid in block_ids) {
    entry <- blocks[[uid]]
    name <- attr(entry, "name")
    if (is.null(name)) name <- uid
    desc <- attr(entry, "description")
    if (is.null(desc)) desc <- ""
    category <- attr(entry, "category")
    if (is.null(category)) category <- "uncategorized"
    package <- attr(entry, "package")
    if (is.null(package)) package <- ""

    options_data[[length(options_data) + 1]] <- list(
      value = uid,
      label = name,
      description = desc,
      category = category,
      package = package,
      icon = blk_icon_name(category), # Use blk_icon_name function
      color = blk_color(category), # Use blk_color function
      searchtext = paste(name, desc, package)
    )
  }

  tagList(
    tags$style(HTML("
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
      .selectize-dropdown .block-option:hover {
        background-color: #f8f9fa;
      }
    ")),
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
        render = I("{
          item: function(item, escape) {
            return '<div>' + escape(item.label) + '</div>';
          },
          option: function(item, escape) {
            var name = escape(item.label);
            var desc = escape(item.description || '');
            var pkg = escape(item.package || '');
            var icon = item.icon || 'circle';
            var color = item.color || '#6c757d';

            var iconWrapper = '<div class=\"block-icon-wrapper\" ' +
                              'style=\"background-color: ' + color + ';\">' +
                              '<i class=\"fa fa-' + icon + '\"></i></div>';
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
        }")
      )
    )
  )
}
