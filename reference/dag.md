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
