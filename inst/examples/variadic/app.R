library(blockr.dag)
library(blockr.core)
library(blockr.dock)

options(
  "g6R.mode" = "dev" #,
  #"g6R.layout_on_data_change" = TRUE
)

serve(
  new_dock_board(
    blocks = c(
      a = new_dataset_block("ChickWeight"),
      b = new_dataset_block("ChickWeight"),
      # variadic
      c = new_rbind_block()
    ),
    links = c(
      new_link(from = "a", to = "c", input = "1"),
      new_link(from = "b", to = "c", input = "2")
    ),
    extensions = new_dag_extension()
  )
)
