
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tessireport

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/brooklynacademyofmusic/tessireport/branch/main/graph/badge.svg)](https://app.codecov.io/gh/brooklynacademyofmusic/tessireport?branch=main)
[![R-CMD-check](https://github.com/brooklynacademyofmusic/tessireport/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/brooklynacademyofmusic/tessireport/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

tessireport is a set of scripts for prediction and auditing Tessitura,
using data queried and cached by tessilake and processed by tessistream.

## Installation

You can install the latest version from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("brooklynacademyofmusic/tessireport")
```

## Example

To run a simple SQL report…

``` r
library(tessireport)

run(sql_report, query = "select * from my_table",
    subject = "This is my table",
    emails = "me@me.com")
```
