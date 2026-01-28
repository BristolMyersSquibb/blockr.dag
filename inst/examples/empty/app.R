library(blockr.dag)
library(blockr.core)
library(blockr.dock)

pkgload::load_all()
pkgload::load_all("../g6r")

# options(
#   "g6R.mode" = "dev",
#   "g6R.layout_on_data_change" = TRUE
# )

serve(
  id = "board",
  new_dock_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_scatter_block(x = "Sepal.Length", y = "Sepal.Width")
    ),
    links = list(from = "a", to = "b", input = "data"),
    extensions = new_dag_extension()
  )
)
