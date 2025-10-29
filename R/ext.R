#' DAG extension
#'
#' Visualizes the DAG (directed acyclic graph) underlying a board and provides
#' UI elemtnts to manipulate the board,
#'
#' @param graph A `graph` object (or `NULL`)
#' @param ... Forwarded to [blockr.dock::new_dock_extension()]
#'
#' @rdname dag
#' @export
new_dag_extension <- function(graph = NULL, ...) {

  blockr.dock::new_dock_extension(
    dag_ext_srv(graph),
    dag_ext_ui,
    name = "DAG",
    class = "dag_extension",
    ...
  )
}

#' @export
context_menu_items.dag_extension <- function(x) {
  list(
    new_context_menu_entry(
      name = "Create edge",
      js = "(value, target, current) => {
        if (current.id === undefined) return;
        const graphId = `${target.closest('.g6').id}`;
        const graph = HTMLWidgets.find(`#${graphId}`).getWidget();
        graph.updateBehavior({
          key: 'create-edge', // Specify the behavior to update
          enable: true,
        });
        // Select node
        graph.setElementState(current.id, 'selected');
        // Disable drag node as it is incompatible with edge creation
        graph.updateBehavior({ key: 'drag-element', enable: false });
        graph.updateBehavior({ key: 'drag-element-force', enable: false });
      }",
      action = NULL, # handled by the 'create-edge' behavior
      condition = function(board, target) {
        target$type == "node"
      },
      id = "create_edge"
    ),
    new_context_menu_entry(
      name = "Remove block",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            if (current.id === undefined) return;
            Shiny.setInputValue('%s', current.id);
          }",
          ns("remove_node")
        )
      },
      action = function(input, output, session, board, update) {
        observeEvent(
          input$remove_node,
          update(list(blocks = list(rm = input$remove_node)))
        )
      },
      condition = function(board, target) {
        target$type == "node"
      }
    ),
    new_context_menu_entry(
      name = "Remove edge",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            if (current.id === undefined) return;
            Shiny.setInputValue('%s', current.id);
            const graphId = `${target.closest('.g6').id}`;
            const graph = HTMLWidgets.find(`#${graphId}`).getWidget();
            graph.removeEdgeData([current.id]);
            graph.draw();
          }",
          ns("remove_edge")
        )
      },
      action = function(input, output, session, board, update) {
        observeEvent(
          input$remove_edge,
          update(list(links = list(rm = input$remove_edge)))
        )
      },
      condition = function(board, target) {
        target$type == "edge"
      }
    ),
    new_context_menu_entry(
      name = "Append block",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            Shiny.setInputValue('%s', current.id, {priority: 'event'});
          }",
          ns("append_block")
        )
      },
      action = function(input, output, session, board, update) {
        ns <- session$ns
        blk <- reactiveVal()

        observeEvent(
          input$append_block,
          {
            blk(NULL)
            showModal(
              create_block_modal(
                mode = "append",
                ns = ns,
                board_block_ids = board_block_ids(board$board),
                board_link_ids = board_link_ids(board$board)
              )
            )
          }
        )

        observeEvent(
          input$append_block_selection,
          {
            req(input$append_block_selection)

            new_blk <- create_block_with_name(
              input$append_block_selection,
              chr_ply(board_blocks(board$board), block_name)
            )

            choices <- block_inputs(new_blk)

            if (is.na(block_arity(new_blk))) {
              opts <- list(create = TRUE)
            } else {
              opts <- list()
            }

            updateSelectizeInput(
              session,
              "append_block_input",
              choices = choices,
              options = opts
            )

            updateTextInput(
              session,
              "append_block_name",
              value = block_name(new_blk)
            )

            blk(new_blk)
          }
        )

        observeEvent(
          input$append_block_confirm,
          {
            blk_id <- input$append_block_id
            lnk_id <- input$append_link_id

            if (!nchar(blk_id) || blk_id %in% board_block_ids(board$board)) {
              notify(
                "Please choose a valid block ID.",
                type = "warning",
                session = session
              )

              return()
            }

            if (!nchar(lnk_id) || lnk_id %in% board_link_ids(board$board)) {
              notify(
                "Please choose a valid link ID.",
                type = "warning",
                session = session
              )

              return()
            }

            new_blk <- blk()

            if (!is_block(new_blk)) {
              notify(
                "Please choose a block type.",
                type = "warning",
                session = session
              )

              return()
            }

            if (!identical(input$append_block_name, block_name(new_blk))) {
              block_name(new_blk) <- input$append_block_name
            }

            new_blk <- as_blocks(set_names(list(new_blk), blk_id))

            new_lnk <- new_link(
              from = input$append_block,
              to = blk_id,
              input = input$append_block_input
            )

            new_lnk <- as_links(set_names(list(new_lnk), lnk_id))

            update(
              list(
                blocks = list(add = new_blk),
                links = list(add = new_lnk)
              )
            )

            removeModal()
          }
        )
      },
      condition = function(board, target) {
        target$type == "node"
      }
    ),
    new_context_menu_entry(
      name = "Create stack",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            Shiny.setInputValue('%s', true, {priority: 'event'});
          }",
          ns("create_stack")
        )
      },
      action = function(input, output, session, board, update) {
        observeEvent(
          input$create_stack,
          {}
        )
      },
      condition = function(board, target) {
        target$type == "canvas"
      }
    ),
    new_context_menu_entry(
      name = "Remove stack",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            if (current.id === undefined) return;
            Shiny.setInputValue('%s', current.id);
          }",
          ns("remove_stack")
        )
      },
      action = function(input, output, session, board, update) {
        observeEvent(
          input$remove_stack,
          {}
        )
      },
      condition = function(board, target) {
        target$type == "combo"
      }
    ),
    new_context_menu_entry(
      name = "Add block",
      js = function(ns) {
        sprintf(
          "(value, target, current) => {
            Shiny.setInputValue('%s', true, {priority: 'event'});
          }",
          ns("add_block")
        )
      },
      action = function(input, output, session, board, update) {
        ns <- session$ns
        blk <- reactiveVal()

        observeEvent(
          input$add_block,
          {
            blk(NULL)
            showModal(
              create_block_modal(
                mode = "add",
                ns = ns,
                board_block_ids = board_block_ids(board$board)
              )
            )
          }
        )

        observeEvent(
          input$add_block_selection,
          {
            req(input$add_block_selection)

            new_blk <- create_block_with_name(
              input$add_block_selection,
              chr_ply(board_blocks(board$board), block_name)
            )

            updateTextInput(
              session,
              "add_block_name",
              value = block_name(new_blk)
            )

            blk(new_blk)
          }
        )

        observeEvent(
          input$add_block_confirm,
          {
            id <- input$add_block_id
            bk <- blk()

            if (!nchar(id) || id %in% board_block_ids(board$board)) {
              notify(
                "Please choose a valid block ID.",
                type = "warning",
                session = session
              )

              return()
            }

            if (!is_block(bk)) {
              notify(
                "Please choose a block type.",
                type = "warning",
                session = session
              )

              return()
            }

            if (!identical(input$add_block_name, block_name(bk))) {
              block_name(bk) <- input$add_block_name
            }

            bk <- as_blocks(set_names(list(bk), id))

            update(list(blocks = list(add = bk)))
            removeModal()
          }
        )
      },
      condition = function(board, target) {
        target$type == "canvas"
      }
    )
  )
}

create_block_with_name <- function(reg_id, blk_nms, ...) {
  name_fun <- function(nms) {
    function(class) {
      last(make.unique(c(nms, default_block_name(class)), sep = " "))
    }
  }

  create_block(reg_id, ..., name = name_fun(blk_nms))
}

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

  # CSS for advanced options toggle (based on blockr.dplyr pattern)
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
    #shiny-modal .modal-body .form-group {
      width: 100%%;
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
    #shiny-modal .modal-footer .btn {
      font-size: 0.875rem;
      padding: 0.375rem 0.75rem;
    }
  ", ns("advanced-options"), ns("advanced-options"))))

  # Always visible fields
  visible_fields <- list(
    block_registry_selectize(ns(selection_id))
  )

  # Add block input field only for append mode (visible)
  if (mode == "append") {
    visible_fields[[length(visible_fields) + 1]] <- selectizeInput(
      ns("append_block_input"),
      "Block input",
      choices = c(`Select a block to populate options` = "")
    )
  }

  # Add block name field (visible)
  visible_fields[[length(visible_fields) + 1]] <- textInput(
    ns(name_id),
    label = "Block name",
    placeholder = "Select block to generate default"
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
  advanced_fields <- list(
    textInput(
      ns(block_id_field),
      label = "Block ID",
      value = rand_names(board_block_ids)
    )
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

  # Add auto-focus script
  auto_focus_script <- tags$script(HTML(sprintf("
    $('#shiny-modal').on('shown.bs.modal', function() {
      $('#%s')[0].selectize.focus();
    });
  ", ns(selection_id))))

  modalDialog(
    title = title,
    tagList(
      advanced_css,
      visible_fields,
      toggle_button,
      advanced_section,
      auto_focus_script
    ),
    footer = tagList(
      tags$button(
        type = "button",
        class = "btn btn-outline-secondary",
        `data-bs-dismiss` = "modal",
        "Cancel"
      ),
      actionButton(ns(confirm_id), button_label, class = "btn-primary")
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
      icon = blk_icon_name(category),  # Use blk_icon_name function
      color = blk_color(category),      # Use blk_color function
      searchtext = paste(name, desc, package)
    )
  }

  tagList(
    tags$style(HTML("
      .block-option {
        padding: 8px 12px;
        display: flex;
        align-items: flex-start;
        gap: 12px;
      }
      .block-icon-wrapper {
        flex-shrink: 0;
        width: 36px;
        height: 36px;
        border-radius: 6px;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 16px;
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
        gap: 8px;
        margin-bottom: 2px;
      }
      .block-name {
        font-weight: 600;
        font-size: 14px;
        color: #212529;
        flex: 1;
      }
      .block-desc {
        font-size: 12px;
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
        margin-bottom: 0px;
        padding-bottom: 8px;
        padding-top: 8px;
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
