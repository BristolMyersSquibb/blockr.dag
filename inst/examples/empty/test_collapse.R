# Test script to verify collapse functionality
library(blockr.dag)
library(blockr.core)

# Load all functions
devtools::load_all()

# Create a simple board with links
board <- new_board(
  blocks = c(
    a = new_dataset_block(),
    b = new_head_block(),
    c = new_head_block()
  ),
  links = c(
    new_link("a", "b", input = "data"),
    new_link("a", "c", input = "data")
  )
)

# Test get_children_from_links
links <- board_links(board)
children <- blockr.dag:::get_children_from_links(links)

cat("Children structure:\n")
print(str(children))
cat("\n")

# Test what update structure looks like
parent_updates <- lapply(names(children), function(parent_id) {
  list(
    id = blockr.dag:::to_g6_node_id(parent_id),
    children = children[[parent_id]]
  )
})

cat("Parent updates structure:\n")
print(str(parent_updates))
