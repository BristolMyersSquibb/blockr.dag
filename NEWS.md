# blockr.dag 0.1.2.9000

## New features

- Locked-mode support. When the board is locked
  (`options(blockr.dock_is_locked = TRUE)`), the DAG extension renders
  as a read-only view of the graph that still allows inspection:
  - `new_toolbar_item()` and `new_context_menu_entry()` gain a
    `show_when_locked` argument (defaulting to `FALSE`). Toolbar
    items / context entries are dropped in locked mode unless this
    flag is set.
  - In the bundled toolbar, only `zoom_in`, `zoom_out`, `auto_fit`
    and `layout` declare `show_when_locked = TRUE`; the context menu
    is empty in locked mode.
  - The g6 canvas keeps `click_select`, `brush_select`,
    `drag_element`, `collapse_expand`, `hover_activate`, zoom and
    pan, so users can still navigate, select and re-arrange the
    graph. Only `create_edge` is dropped (it mutates the board).
  - Clicking a node no longer opens a block panel in the dock
    (the `selected_node` observer is gated with
    `blockr.dock::req_unlocked()`).
  - Backspace, Ctrl+C/X/V keyboard shortcuts are not bound (cut and
    paste mutate; copy is also disabled to avoid leaking board state
    outside the locked context).
  - The empty-state placeholder shows a "Workflow is read-only"
    message instead of the editing hints.
  - As a server-side trust boundary, every state-mutating
    `observeEvent` trigger is wrapped with
    `blockr.dock::req_unlocked()` so a forged
    `Shiny.setInputValue()` cannot bypass the UI filtering.

## Internal changes

- Label observers for OTEL support.
- Add support for collapsible nodes and combos, through g6R.
- Reworked actions. Inherits from `blockr.dock`.
- Added support for node ports from g6R.
- Fix [#86](https://github.com/BristolMyersSquibb/blockr.dag/issues/86).
- Fix [#110](https://github.com/BristolMyersSquibb/blockr.dag/issues/110): copy/cut keyboard shortcuts no longer hijack plain text selections (column names, error messages, etc.).

# blockr.dag 0.1.0

- Initial CRAN submission.
