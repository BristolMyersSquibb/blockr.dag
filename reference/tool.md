# Toolbar item functions

Functions for creating and working with toolbar items for the DAG
interface.

## Usage

``` r
new_toolbar_item(id, icon, js, action = NULL, tooltip = NULL)

is_toolbar_item(x)

toolbar_items(x)
```

## Arguments

- id:

  Unique identifier for the toolbar item.

- icon:

  Name of an icon to show in the toolbar.

- js:

  JavaScript code to execute when the entry is selected.

- action:

  Action to perform when the entry is selected.

- tooltip:

  Optional tooltip text for the entry.

- x:

  Object

## Value

- `new_toolbar_item()`:

  A toolbar item object of class "toolbar_item" containing action and js
  functions, with id, icon, and tooltip attributes.

- `is_toolbar_item()`:

  `TRUE` if `x` is a toolbar item, `FALSE` otherwise.

- `toolbar_items()`:

  A list of toolbar items for the given object.

## Details

- `new_toolbar_item()`:

  Creates a new toolbar item with the specified id, icon, JavaScript
  code, action function, and tooltip text.

- `is_toolbar_item()`:

  Tests whether an object is a valid toolbar item.

- `toolbar_items()`:

  Generic function to extract toolbar items from various objects like
  dock extensions, boards, or lists.

The `toolbar_items.dag_extension()` method provides the following
actions:

- Zoom in - Increases the graph zoom level.

- Zoom out - Decreases the graph zoom level.

- Auto fit - Automatically fits the entire graph within the viewport.

- Layout - Reapplies the graph layout algorithm to reorganize nodes.

- Add block - Opens interface to add a new block to the workflow.

- Add stack - Creates a new workflow stack.

- Remove selected - Removes currently selected elements from the graph.
