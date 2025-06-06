
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tessireport

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/skysyzygy/tessireport/branch/master/graph/badge.svg)](https://codecov.io/gh/skysyzygy/tessireport?branch=master)
[![R-CMD-check](https://github.com/brooklynacademyofmusic/tessireport/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/brooklynacademyofmusic/tessireport/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

tessireport is a set of scripts for data hygiene in Tessitura, using
data queried and cached by tessilake and processed by tessistream.

## Installation

You can install the latest version from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("skysyzygy/tessireport")
```

## Example

To run a simple SQL report…

``` r
library(tessireport)

run(sql_report, query = "select * from my_table",
    subject = "This is my table",
    emails = "me@me.com")
```
