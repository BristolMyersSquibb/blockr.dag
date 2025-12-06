library(blockr.dag)
library(blockr.dock)
library(blockr.core)

graph <- new_graph(
  nodes = list(
    list(id = 1, style = list(labelText = "Node 1")),
    list(id = 2, style = list(labelText = "Node 2"))
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
