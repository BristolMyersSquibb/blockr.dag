# DAG extension

Visualizes the DAG (directed acyclic graph) underlying a board and
provides UI elements to manipulate the board.

## Usage

``` r
new_dag_extension(graph = NULL, ...)
```

## Arguments

- graph:

  A `graph` object (or `NULL`).

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
