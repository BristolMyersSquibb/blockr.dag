
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blockr.dag

<!-- badges: start -->

<!-- badges: end -->

An interative network library provided by g6R can be used as front-end
to a blockr board using this package.

## Installation

You can install the development version of blockr.dag from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("BristolMyersSquibb/blockr.dag")
```

## Example

To start up a board with the `dag` extension, run the following code:

``` r
library(blockr.dag)
library(blockr.dock)
library(blockr.core)

serve(
  new_dock_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_scatter_block(x = "Sepal.Length", y = "Sepal.Width")
    ),
    links = list(from = "a", to = "b", input = "data"),
    stacks = list(stack_1 = c("a", "b")),
    extensions = new_dag_extension()
  )
)
## basic example code
```

To start up the dag extension with dummy nodes and edges:

``` r
library(blockr.dag)
library(blockr.dock)
library(blockr.core)

graph <- new_graph(
  nodes = list(
    list(id = 1, label = "Node 1"),
    list(id = 2, label = "Node 2")
  ),
  edges = list(
    list(
      source = 1,
      target = 2,
      style = list(
        labelText = "Edge from 1 to 2"
      )
    )
  )
)

serve(
  new_dock_board(
    extensions = new_dag_extension(graph)
  )
)
## basic example code
```
