# Context Menu Items

``` r
library(blockr.dag)
```

The **context menu** provides **actions** based on what element is
**right-clicked** in the graph.

## Available Actions

[TABLE]

## Creating Context Menu Entries

Context menu items are created with
[`new_context_menu_entry()`](https://bristolmyerssquibb.github.io/blockr.dag/reference/ctx.md):

``` r
# Context menu entry
entry <- new_context_menu_entry(
  name = "Custom Action",
  js = function(ns) "alert('Custom action')",
  action = function(board, update, ...) {
    # Server-side logic
  },
  condition = function(board, target) {
    target$type == "node"
  }
)
```

`js` can be a string representing a JavaScript function or an R function
that takes the namespace `ns` as input and returns a string:

``` r
function(ns) {
  sprintf(
    "(value, target, current) => {
      Shiny.setInputValue('%s', true, {priority: 'event'});
    }",
    ns("add_block")
  )
}
```

`condition` is an either a boolean or an R function that takes the
`board` and `target` (the clicked element) as input and returns `TRUE`
or `FALSE` to determine if the entry should be shown.

Any new entry has to go inside `context_menu_items.dag_extension` for
registration:

``` r
# Add custom context menu items to an extension
context_menu_items.dag_extension <- function(x) {
  list(
    new_context_menu_entry(
      name = "My Action",
      js = function(ns) {
        sprintf("Shiny.setInputValue('%s', true)", ns("my_action"))
      },
      action = my_custom_action("my_action"),
      condition = function(board, target) TRUE
    )
  )
}
```
