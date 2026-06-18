# Changelog

## blockr.dag 0.1.2.9000

### Internal changes

- Label observers for OTEL support.
- Add support for collapsible nodes and combos, through g6R.
- Reworked actions. Inherits from `blockr.dock`.
- Added support for node ports from g6R.
- Reworked block ports: ports are now always visible (rather than
  hover-only) with a fixed radius, and the output port uses the
  `label-bottom` placement. Pairs with g6R’s port-grab tolerance so a
  near-miss on a port still starts an edge. Requires
  `g6R (>= 0.6.0.9000)`.
- Fix
  [\#86](https://github.com/BristolMyersSquibb/blockr.dag/issues/86).
- Fix
  [\#110](https://github.com/BristolMyersSquibb/blockr.dag/issues/110):
  copy/cut keyboard shortcuts no longer hijack plain text selections
  (column names, error messages, etc.).
- Fix
  [\#123](https://github.com/BristolMyersSquibb/blockr.dag/issues/123):
  renaming a block now relabels its DAG node instead of erroring.
  `update_observer()` handed blockr.core’s partial-argument `blocks$mod`
  delta straight to `update_nodes()` (which needs full `block` objects);
  it now updates the node label directly from the delta.

## blockr.dag 0.1.0

CRAN release: 2025-12-18

- Initial CRAN submission.
