# DAG Extension Example
#
# Simple example showing the DAG extension with one block.
# Use the context menu to add more blocks and create connections.

library(blockr.core)
library(blockr.dock)
library(blockr.dag)

serve(
  new_dock_board(
    blocks = c(data = new_dataset_block("iris")),
    extensions = new_dag_extension()
  )
)
