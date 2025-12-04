# blockr.dag


<!-- README.md is generated from README.Rmd. Please edit that file -->

# blockr.dag

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![status](https://github.com/BristolMyersSquibb/blockr.dag/actions/workflows/ci.yaml/badge.svg)](https://github.com/BristolMyersSquibb/blockr.dag/actions/workflows/ci.yaml)
[![coverage](https://codecov.io/gh/BristolMyersSquibb/blockr.dag/graph/badge.svg?token=s0zz3En4x1)](https://app.codecov.io/gh/BristolMyersSquibb/blockr.dag)
[![CRAN
status](https://www.r-pkg.org/badges/version/blockr.dag)](https://CRAN.R-project.org/package=blockr.dag)
<!-- badges: end -->

An interative network library provided by g6R can be used as front-end
to a blockr board using this package.

## Installation

You can install the development version of blockr.dag from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("BristolMyersSquibb/blockr.dag")
```

## Example

To start up a board with the `dag` extension, run the following code:

``` r
library(blockr.dag)
library(blockr.core)
library(blockr.dock)

options(
  "g6R.mode" = "dev",
  #"g6R.layout_on_data_change" = TRUE,
  "g6R.preserve_elements_position" = TRUE
)

serve(
  new_dock_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_scatter_block(x = "Sepal.Length", y = "Sepal.Width")
    ),
    links = list(from = "a", to = "b", input = "data"),
    stacks = c(
      stack_1 = new_dock_stack(c("a", "b"), color = "#0000FF"),
      stack_2 = new_dock_stack()
    ),
    extensions = new_dag_extension()
  )
)
```

> **Note**
>
> The demo below runs with shinylive. Not all feature may work as
> expected due to compatibility issues with webR.

<iframe class="border border-5 rounded shadow-lg" src="https://shinylive.io/r/app/#h=0&amp;code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAdzgCMAnRRAVwhiLdIAoAdMAPQsAzgwEAbAJZ0BWCdIZQGATwF1xRAgGsGGYgzj8AlLwi1GzSRGGko48XzDrNOvUQP9cAAgOoiwrwBeL34AC1JSVGFEAQECZQgdKAwAc0lSUJY6DEkiNQ1tXXM5Y1NzJkQrGzsHfmdCjAATKBTPHzg-AOCwiKiYuISk1PTM7Nz8lyL6ErATCClGJWVuetdmlLmFxRUVgtd9OE2FJd3Jppc50yJUUlzrPggvELAUgDYsDA5GwzAg5++AG6eUxPADE-DeH3EUGUnFIAH0SPDmrZ4QRQlAICkfn8ACpYACqAFFcCDnpCMKgDMI4AwAXB4XBxHB4GRhPDOuk7vw8YSiaZLtZafSHk8IHBqMiXPC6EQlI1RU8vKsul4CIqlV4oH9xZKUVAaQjVo5JAxJMJjKTHprlTqJfDhAQoBFaTK9twAB5-fgAZQ6dgwABkKCkMm1lN6wH7UAGAOqSRph2Zkp5GK2aqSJVVSGzcABmDCIMEjUDapCIkbobSsqBYpEj+stKa81W0qvVzaera08IAjHa9dLu9wOyg2nVLWqiBoGJHQQAGRfzgBiy6b1s13fhACYB1LtA7bNpuHNNWnm3APeRrHdVbrkS1GVeKMI7ieyXMjGAAL4AXSAA" style="zoom: 0.75;" width="100%" height="1100px"></iframe>
