# Context menu functions

Functions for creating and working with context menu entries.

## Usage

``` r
new_context_menu_entry(
  name,
  js,
  action = NULL,
  condition = TRUE,
  id = tolower(gsub(" +", "_", name))
)

is_context_menu_entry(x)

context_menu_items(x)
```

## Arguments

- name:

  Name of the context menu entry.

- js:

  JavaScript code to execute when the entry is selected.

- action:

  Action to perform when the entry is selected.

- condition:

  Condition to determine if the entry should be shown.

- id:

  Unique identifier for the context menu entry. Inferred from `name` if
  not provided

- x:

  Object

## Value

- `new_context_menu_entry()`:

  A context menu entry object of class "context_menu_entry" containing
  condition, action, and js functions, with name and id attributes.

- `is_context_menu_entry()`:

  `TRUE` if `x` is a context menu entry, `FALSE` otherwise.

- `context_menu_items()`:

  A list of context menu items for the given object.

## Details

- `new_context_menu_entry()`:

  Creates a new context menu entry with the specified name, JavaScript
  code, action function, and display condition.

- `is_context_menu_entry()`:

  Tests whether an object is a valid context menu entry.

- `context_menu_items()`:

  Generic function to extract context menu items from various objects
  like dock extensions, boards, or lists.

The `context_menu_items.dag_extension()` method provides the following
actions:

- Create link - Creates connections between workflow nodes.

- Remove block - Removes individual blocks from the workflow.

- Remove link - Removes connections between workflow nodes.

- Append block - Adds a new block after the selected node.

- Create stack - Creates a new workflow stack.

- Remove stack - Removes an entire workflow stack.

- Edit stack - Opens stack editing interface.

- Add block - Adds a new block to the canvas.
