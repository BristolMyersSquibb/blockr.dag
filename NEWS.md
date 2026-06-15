# blockr.dag 0.1.2.9000

## Internal changes

- Label observers for OTEL support.
- Add support for collapsible nodes and combos, through g6R.
- Reworked actions. Inherits from `blockr.dock`.
- Added support for node ports from g6R.
- Fix [#86](https://github.com/BristolMyersSquibb/blockr.dag/issues/86).
- Fix [#110](https://github.com/BristolMyersSquibb/blockr.dag/issues/110): copy/cut keyboard shortcuts no longer hijack plain text selections (column names, error messages, etc.).
- Fix [#123](https://github.com/BristolMyersSquibb/blockr.dag/issues/123): renaming a block now relabels its DAG node instead of erroring. `update_observer()` handed blockr.core's partial-argument `blocks$mod` delta straight to `update_nodes()` (which needs full `block` objects); it now updates the node label directly from the delta.
- Fix [#127](https://github.com/BristolMyersSquibb/blockr.dag/issues/127): dragging an edge from a node port onto the canvas only triggered the append/prepend action intermittently ("every second time"). The `added_edge` input was set without `{priority: "event"}`, so a repeat drag with an identical payload was de-duplicated by Shiny and the observer never fired. The canvas-drop handler also silently discarded the gesture via `req(edge$portType)`. Both `added_edge` emits now use event priority, and the handler defaults to append (output port) instead of bailing.

# blockr.dag 0.1.0

- Initial CRAN submission.
