library(blockr.dag)
library(blockr.core)
library(blockr.dock)

options(
  "g6R.mode" = "dev",
  #"g6R.layout_on_data_change" = TRUE,
  "g6R.preserve_elements_position" = TRUE
)

serve(
  new_dock_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_scatter_block(x = "Sepal.Length", y = "Sepal.Width")
    ),
    links = list(from = "a", to = "b", input = "data"),
    stacks = c(
      stack_1 = new_dag_stack(c("a", "b"), color = "#0000FF"),
      stack_2 = new_dag_stack()
    ),
    extensions = new_dag_extension()
  )
)
