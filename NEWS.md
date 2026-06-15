# blockr.dag 0.1.2.9000

## Internal changes

- Label observers for OTEL support.
- Add support for collapsible nodes and combos, through g6R.
- Reworked actions. Inherits from `blockr.dock`.
- Added support for node ports from g6R.
- Fix [#86](https://github.com/BristolMyersSquibb/blockr.dag/issues/86).
- Fix [#110](https://github.com/BristolMyersSquibb/blockr.dag/issues/110): copy/cut keyboard shortcuts no longer hijack plain text selections (column names, error messages, etc.).
- Fix [#123](https://github.com/BristolMyersSquibb/blockr.dag/issues/123): renaming a block now relabels its DAG node instead of erroring. `update_observer()` handed blockr.core's partial-argument `blocks$mod` delta straight to `update_nodes()` (which needs full `block` objects); it now updates the node label directly from the delta.
- Dropped the assist-node probe from the `drag_element` `enable` predicate. g6R ([cynkra/g6R#49](https://github.com/cynkra/g6R/pull/49)) now pauses node dragging during port edge-creation and restores the consumer predicate on drop, so the probe was redundant. The predicate is back to just suppressing drag on `shift`/`alt`. Requires `g6R (>= 0.6.0.9000)`.

# blockr.dag 0.1.0

- Initial CRAN submission.
