# DAG Extension Example
#
# Simple example showing the DAG extension with one block.
# Use the context menu to add more blocks and create connections.

library(blockr.core)
library(blockr.dock)
library(blockr.ggplot)
pkgload::load_all(".")

serve(
  new_dock_board(
    blocks = list(
      data = new_dataset_block("iris"),
      plot = new_ggplot_block(),
      transform = new_arrange_block()
    ),
    extensions = new_dag_extension()
  )
)
