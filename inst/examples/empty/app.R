library(blockr.dag)
library(blockr.core)
library(blockr.dock)

options(
  "g6R.mode" = "dev",
  "g6R.layout_on_data_change" = TRUE
)

serve(
  id = "board",
  new_dock_board(
    extensions = new_dag_extension()
  )
)
