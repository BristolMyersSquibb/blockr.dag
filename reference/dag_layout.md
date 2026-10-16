# DAG layout

How the Workflow extension arranges the board: the layered DAG layout,
its flow direction and its spacing. Users change both from the toolbar's
layout menu;
[`new_dag_extension()`](https://bristolmyerssquibb.github.io/blockr.dag/reference/dag.md)
takes the one a board starts with.

## Usage

``` r
dag_layout(
  direction = c("TB", "LR"),
  spacing = c("normal", "compact", "loose")
)

is_dag_layout(x)
```

## Arguments

- direction:

  `"TB"` (top to bottom) or `"LR"` (left to right). A wide board turned
  sideways grows in height instead.

- spacing:

  `"compact"`, `"normal"` or `"loose"`: the gaps between blocks and
  between layers.

- x:

  Object to test.

## Value

`dag_layout()` returns an object of class `dag_layout`: a list of
`direction` and `spacing`, plain values only, so it round-trips through
serialization.

## Examples

``` r
dag_layout()
#> $direction
#> [1] "TB"
#> 
#> $spacing
#> [1] "normal"
#> 
#> attr(,"class")
#> [1] "dag_layout"
dag_layout(direction = "LR", spacing = "compact")
#> $direction
#> [1] "LR"
#> 
#> $spacing
#> [1] "compact"
#> 
#> attr(,"class")
#> [1] "dag_layout"
```
