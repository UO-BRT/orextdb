
<!-- README.md is generated from README.Rmd. Please edit that file -->

# orextdb

<!-- badges: start -->

[![R-CMD-check](https://github.com/UO-BRT/orextdb/workflows/R-CMD-check/badge.svg)](https://github.com/UO-BRT/orextdb/actions)
<!-- badges: end -->

The goal of **orextdb** is to make data extraction into R easy.

## Installation

``` r
# install.packages("remotes") # if not previously installed
remotes::install_github("UO-BRT/orextdb")
```

## Keys

You first need to make sure you have a key to access the web API housing
the data. The **orextdb** package includes helper functions for working
with your key. First, store your key in your `.Renviron` using the
following code, swapping the text for your specific key.

``` r
library(orextdb)
db_set_key(key = "mykeyabcdefg")
```

This will write a key to your `.Renviron` that is accessible via
`db_key()`. You should only need to do this once, and you will need to
make sure you restart your R session for the change to take effect.
Generally, you won’t need to access your key, but if for any reason you
do, you can always run `db_key()` and your key will be printed to the
console.

## Accessing data

Once you’ve set your key, you can access any of the tables in the Oregon
Extended Database via `db_get()`. Just supply the name of the table you
want, and it will be returned. If you don’t know the name of the table
you want, you can provide any text you want and the available tables
will be printed for you.

``` r
db_get("hardyharhar")
#> Error: The table you requested is not part of the database. Please request one of the following tables:
#> Accomodations
#> Answers
#> Districts
#> Exams
#> Items
#> Preferences
#> Schools
#> Students
#> Students_old
#> Submissions
#> SupplementalDistricts
#> SupplementalSchools
#> Tasks
#> User
#> UserStudents
#> UserStudents_old
```

Eventually, functions will be built that combine tables in more useful
formats, but currently it just returns the tables in their raw form.
