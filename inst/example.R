# library(blockr.dag)
# library(blockr.dock)
library(blockr.core)
pkgload::load_all("../blockr.dock")
pkgload::load_all("../blockr.dag")
library(blockr.dplyr)

serve(
  new_dock_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_scatter_block(x = "Sepal.Length", y = "Sepal.Width")
    ),
    links = list(from = "a", to = "b", input = "data"),
    extensions = new_dag_extension()
  )
)
