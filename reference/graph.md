# Graph object

R list-based representation of a g6 graph object.

## Usage

``` r
new_graph(nodes = list(), edges = list(), combos = list())

is_graph(x)

graph_nodes(x)

graph_edges(x)

graph_combos(x)

as_graph(x, ...)

# S3 method for class 'graph'
as_graph(x, ...)

# S3 method for class 'list'
as_graph(x, ...)
```

## Arguments

- nodes:

  Graph nodes (i.e. board blocks).

- edges:

  Graph edges (i.e. board links).

- combos:

  Node groups (i.e. board stacks).

- x:

  Object to test or convert.

- ...:

  Generic consistency.

## Value

- `new_graph()`:

  A graph object of class "graph" containing nodes, edges, and combos
  lists.

- `is_graph()`:

  `TRUE` if `x` is a graph object, `FALSE` otherwise.

- `graph_nodes()`:

  A list of graph nodes.

- `graph_edges()`:

  A list of graph edges.

- `graph_combos()`:

  A list of graph combos (node groups).

- `as_graph()`:

  A graph object converted from the input.

## Details

- `new_graph()`:

  Creates a new graph object with the specified nodes, edges, and
  combos.

- `is_graph()`:

  Tests whether an object is a valid graph object.

- `graph_nodes()`:

  Extracts the nodes component from a graph object.

- `graph_edges()`:

  Extracts the edges component from a graph object.

- `graph_combos()`:

  Extracts the combos component from a graph object.

- `as_graph()`:

  Generic function to convert objects to graph format.
