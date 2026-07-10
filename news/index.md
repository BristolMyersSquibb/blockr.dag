# Changelog

## blockr.dag 0.1.2.9000

### Breaking changes

- The DAG extension no longer ingests or emits a full g6 graph object
  ([\#119](https://github.com/BristolMyersSquibb/blockr.dag/issues/119)).
  [`new_dag_extension()`](https://bristolmyerssquibb.github.io/blockr.dag/reference/dag.md)
  drops the `graph` argument in favour of `positions`, a named list
  keyed by block id (`list(a = list(x = 100, y = 200))`) carrying only
  the board-independent view attributes the extension owns. The board is
  now the single source of truth: nodes, edges, combos and all styling
  (icons, labels, colors, ports) are always regenerated from it, and
  supplied positions are overlaid onto the corresponding nodes’
  coordinates. Serialization shrinks to positions only; no board-derived
  styling is persisted. The g6 graph wire format and its board/g6
  converters (`new_graph()`, `as_graph()`, `g6_from_graph()`, …) are now
  internal and no longer exported. This mirrors the dockview wire-format
  decoupling in `blockr.dock`. Note: the auto-layout still computes
  final node placement at cold start, so supplied positions are not yet
  honored over it; making positions pin over the layout is a planned
  follow-up (see
  [\#141](https://github.com/BristolMyersSquibb/blockr.dag/issues/141)).

### New features

- The node status badge reflects a block’s full eval status, not just
  error conditions
  ([\#145](https://github.com/BristolMyersSquibb/blockr.dag/issues/145)).
  The badge is derived by
  [`blockr.dock::block_status_badge()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/meta.html)
  – the single derivation the dock card icon and the DAG node badge
  share – so a node shows the same amber `waiting`, yellow `unset` or
  red `failed` dot as the dock, with identical colour and geometry. Any
  error condition still shows red (a render-phase error leaves a block
  `ready` but errored). A `ready` node carries no badge; a `dormant`
  node – one whose status is not currently computed because nothing
  renders its output – keeps its last-known badge rather than dropping
  it, so every invalid block stays flagged even after it leaves the eval
  set.

- Node positions are externally controllable
  ([\#120](https://github.com/BristolMyersSquibb/blockr.dag/issues/120)).
  The `positions` handle is registered via `external_ctrl`, so positions
  can be set programmatically (e.g. by an AI assistant) through the
  board update lifecycle:
  `update(list(extensions = list(mod = list(<ext_id> = list(positions = list(<block-id> = list(x = , y = )))))))`
  moves the corresponding nodes. The handle is bidirectional: it tracks
  live user drags (debounced) and pushes external writes to the client,
  with a whole-pixel-rounded equality guard that prevents the set/echo
  feedback loop.

### Internal changes

- The DAG now renders with G6’s default canvas renderer instead of the
  SVG renderer. The SVG element reports `offsetWidth == 0`, which broke
  `g-lite`’s client/canvas coordinate scaling under browser zoom other
  than 100% (drops and port grabs silently failed below 100%). The SVG
  renderer is still used for `shinytest2` end-to-end tests via the new
  `blockr.dag.svg_renderer` option (see
  [`?new_dag_extension`](https://bristolmyerssquibb.github.io/blockr.dag/reference/dag.md)).
  Requires `g6R (>= 0.6.0.9001)`, which keeps the create-edge assist
  node from crashing the canvas renderer.
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
- Fix
  [\#144](https://github.com/BristolMyersSquibb/blockr.dag/issues/144):
  copy/paste no longer corrupts `NULL`-valued block state.
  `copy_selection_to_clipboard()` serialized the clipboard with
  `toJSON()`’s default `null = "list"`, encoding `NULL` as
  [`{}`](https://rdrr.io/r/base/Paren.html) and deserializing it back
  into [`list()`](https://rdrr.io/r/base/list.html); pasted blocks then
  errored on first data (e.g. `nzchar(list())` -\> “argument is of
  length zero” in the chart and table blocks). Now passes
  `null = "null"` to match blockr.core’s save/restore serialization,
  round-tripping `NULL` faithfully.
- Fix
  [blockr.dock#308](https://github.com/BristolMyersSquibb/blockr.dock/issues/308):
  single-clicking a DAG node opens the block’s panel again.
  `blockr.dock` retired the live `dock` handle from the extension server
  surface, which the node-select observer passed to
  `blockr.dock::show_panel()`. The observer now emits a `views` update
  delta built with `blockr.dock`’s panel-op grammar (`select`, or
  `add` + `select` when absent) to reveal the block’s panel in the
  current view, adding it there if the view does not already hold it —
  without switching to another view.
- Track `blockr.dock`’s extension-identity redesign. An extension’s
  runtime id is now the container-assigned key (its class minus the
  `_extension` suffix, or an explicit list name), and extension results
  reach actions and block callbacks as one `extensions` bundle keyed by
  that id rather than a splatted argument named after the id. The DAG
  actions and the block-status callback now resolve their result with
  `blockr.dock::extension_ids(board$board, "dag_extension")` and index
  the bundle explicitly, instead of binding a `dag_extension` formal.

## blockr.dag 0.1.0

CRAN release: 2025-12-18

- Initial CRAN submission.
