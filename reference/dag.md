# DAG extension

Visualizes the DAG (directed acyclic graph) underlying a board and
provides UI elements to manipulate the board.

## Usage

``` r
new_dag_extension(positions = NULL, ...)
```

## Arguments

- positions:

  Optional node positions overlaid on the board-derived nodes, as a
  named list keyed by block id, each element a list with numeric `x` and
  `y` (e.g. `list(a = list(x = 100, y = 200))`). Persisted across save /
  restore. Unknown or stale block ids are ignored. This handle is
  externally controllable: positions can be set programmatically through
  the board update lifecycle
  (`update(list(extensions = list(mod = list(<ext_id> = list(positions = ...)))))`),
  which moves the corresponding nodes. Note: the auto-layout currently
  computes final node placement at cold start, so supplied positions are
  not yet honored over it (a follow-up will let positions pin over the
  layout).

- ...:

  Forwarded to
  [`blockr.dock::new_dock_extension()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/extension.html).

## Value

A `dag_extension` object that extends the dock extension system for
visualizing and manipulating DAG workflows.

## Options

`blockr.dag.svg_renderer`: when `TRUE`, the DAG is rendered with the SVG
renderer instead of the default canvas renderer. Canvas is the default
because the SVG element reports `offsetWidth == 0`, which makes the
underlying `g-lite` client/canvas coordinate scaling ignore the page
zoom factor and desyncs hit-testing below 100% browser zoom. The SVG
renderer keeps every element in the DOM, which the `shinytest2`
end-to-end tests need to query and screenshot, so they opt in via
`AppDriver$new(options = list(blockr.dag.svg_renderer = TRUE))`.
