library(blockr.core)
library(blockr.dag)

serve(
  new_dag_board(
    blocks = c(
      data = new_dataset_block("iris"),
      sub = new_subset_block(),
      head = new_head_block(n = 10L),
      plot = new_scatter_block(x = "Sepal.Length", y = "Sepal.Width")
    ),
    links = c(
      new_link("data", "sub"),
      new_link("sub", "head"),
      new_link("data", "plot")
    )
  )
)
