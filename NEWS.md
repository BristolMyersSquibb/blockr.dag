# blockr.dag 0.1.2.9000

## Breaking changes

- The DAG extension no longer ingests or emits a full g6 graph object ([#119](https://github.com/BristolMyersSquibb/blockr.dag/issues/119)). `new_dag_extension()` drops the `graph` argument in favour of `positions`, a named list keyed by block id (`list(a = list(x = 100, y = 200))`) carrying only the board-independent view attributes the extension owns. The board is now the single source of truth: nodes, edges, combos and all styling (icons, labels, colors, ports) are always regenerated from it, and supplied positions are overlaid onto the corresponding nodes' coordinates. Serialization shrinks to positions only; no board-derived styling is persisted. The g6 graph wire format and its board/g6 converters (`new_graph()`, `as_graph()`, `g6_from_graph()`, ...) are now internal and no longer exported. This mirrors the dockview wire-format decoupling in `blockr.dock`. Note: the auto-layout still computes final node placement at cold start, so supplied positions are not yet honored over it; making positions pin over the layout is a planned follow-up (see [#141](https://github.com/BristolMyersSquibb/blockr.dag/issues/141)).

## Internal changes

- The DAG now renders with G6's default canvas renderer instead of the SVG renderer. The SVG element reports `offsetWidth == 0`, which broke `g-lite`'s client/canvas coordinate scaling under browser zoom other than 100% (drops and port grabs silently failed below 100%). The SVG renderer is still used for `shinytest2` end-to-end tests via the new `blockr.dag.svg_renderer` option (see `?new_dag_extension`). Requires `g6R (>= 0.6.0.9001)`, which keeps the create-edge assist node from crashing the canvas renderer.
- Label observers for OTEL support.
- Add support for collapsible nodes and combos, through g6R.
- Reworked actions. Inherits from `blockr.dock`.
- Added support for node ports from g6R.
- Reworked block ports: ports are now always visible (rather than hover-only) with a fixed radius, and the output port uses the `label-bottom` placement. Pairs with g6R's port-grab tolerance so a near-miss on a port still starts an edge. Requires `g6R (>= 0.6.0.9000)`.
- Fix [#86](https://github.com/BristolMyersSquibb/blockr.dag/issues/86).
- Fix [#110](https://github.com/BristolMyersSquibb/blockr.dag/issues/110): copy/cut keyboard shortcuts no longer hijack plain text selections (column names, error messages, etc.).
- Fix [#123](https://github.com/BristolMyersSquibb/blockr.dag/issues/123): renaming a block now relabels its DAG node instead of erroring. `update_observer()` handed blockr.core's partial-argument `blocks$mod` delta straight to `update_nodes()` (which needs full `block` objects); it now updates the node label directly from the delta.

# blockr.dag 0.1.0

- Initial CRAN submission.
