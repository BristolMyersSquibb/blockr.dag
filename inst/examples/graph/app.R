library(blockr.dag)
library(blockr.dock)
library(blockr.core)

# The board is the single source of truth for nodes / edges / combos. The DAG
# extension only owns board-independent view attributes, e.g. node positions,
# supplied as a named list keyed by block id and persisted across save /
# restore. (The auto-layout still computes final placement at cold start, so
# positions are not yet honored over it; pinning is a planned follow-up.)
positions <- list(
  a = list(x = 200, y = 150),
  b = list(x = 200, y = 350)
)

serve(
  new_dock_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_head_block()
    ),
    links = list(from = "a", to = "b", input = "data"),
    extensions = new_dag_extension(positions = positions)
  )
)
