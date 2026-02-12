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
      a = new_dataset_block("iris"),
      b = new_head_block(n = 10),
      c = new_subset_block(),
      d = new_head_block(n = 5),
      e = new_rbind_block(),
      f = new_subset_block(),
      g = new_rbind_block(),
      h = new_head_block(n = 3),
      i = new_scatter_block(x = "Sepal.Length", y = "Sepal.Width")
    ),
    links = c(
      new_link(from = "a", to = "b", input = "data"),
      new_link(from = "b", to = "c", input = "data"),
      new_link(from = "c", to = "d", input = "data"),
      new_link(from = "b", to = "f", input = "data"),
      new_link(from = "d", to = "e", input = "1"),
      new_link(from = "f", to = "e", input = "2"),
      new_link(from = "e", to = "g", input = "1"),
      new_link(from = "f", to = "g", input = "2"),
      new_link(from = "g", to = "h", input = "data"),
      new_link(from = "h", to = "i", input = "data")
    ),
    extensions = new_dag_extension()
  )
)
