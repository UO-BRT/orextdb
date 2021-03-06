
<!-- README.md is generated from README.Rmd. Please edit that file -->

# orextdb

The `{orextdb}` package interfaces with the Oregon Extended Assessment
(ORExt) API which pulls from the live ORExt database to simplify access
to the data. This package is primarily used in conjunction with
`{dbprocess}` & `{exirt}`. This vignette explains the main functions in
`{orext}` to help someone building out the aforementioned packages, or
directly pulling data for another reason. The goal of `{orext}` is to
make data extraction into R easy.

<!-- badges: start -->

[![R-CMD-check](https://github.com/UO-BRT/orextdb/workflows/R-CMD-check/badge.svg)](https://github.com/UO-BRT/orextdb/actions)
[![Codecov test
coverage](https://codecov.io/gh/UO-BRT/orextdb/branch/main/graph/badge.svg)](https://app.codecov.io/gh/UO-BRT/orextdb?branch=main)
<!-- badges: end -->

## Accessing the Data Base

You can check if you have access with `db_key()`. If this returns an
empty string, you do not have the key. You can set the key by wrapping
the key in quotes within `db_set_key()` (e.g.,
`db_set_key('asdfjkl-123456')`)

Databases are specified with the prefix “ORExt” and then the final two
digits of the academic year. So for 2018-19, we specify
`db = 'ORExt1819'`.

We also have to specify which table we want. If we want to estimate the
ability estimates, we need to specify which table we want pulled from
the database.

This is done with the argument `table =`, which is a string specifying
the specific table from the Oregon Extended live database.

`table` should be one of

    * "Accomodations"
    * "Answers"
    * "Districts" 
    * "Exams"
    * "Items" 
    * "Preferences" 
    * "Schools"
    * "Students" 
    * "Students_old" 
    * "Submissions" 
    * "SupplementalDistricts"
    * "SupplementalSchools"
    * "Tasks"
    * "User" 
    * "UserStudents"
    * "UserStudents_old"

Check the documentation with `?db_get()` for further arguments, though
defaults have been set to be useful and will not need to be changed
typically.

# Load the library

As with any package, we need to load the library. If you do not have the
package installed, you can use
`remotes::install_github('UO-BRT/orextdb')` before this step, assuming
you have access to the repository.

``` r
library(orextdb)
```

# Check current data base

We can see the current database and the general formatting of database
names with this call.

``` r
current_db()
#> [1] "ORExt2122"
```

## internal workings of `current_db()`

This function is used internally if the user does not specify a specific
database, so this is the database the package assumes if unspecified.
The current database switches from \[current year - 1, current year\] to
\[current year, year + 1\] after September 1st. For example: calling
`current_db()` on August 31, 2022 returns `ORExt2122`, and calling it on
September 1, 2022 returns `ORExt2223`. With additional functionality in
the API, new functions will be added to list all available databases.

## check formatting of database name

As you can see, databases are formatted with the prefix `ORExt` followed
by the academic year. Academic year is specified as the last two digits
of each year in the academic year (e.g., 2018-2019 = `1819`, 2019-2020 =
`1920`, etc.). Hence, the database for the 2018-2019 OR Extended
Assessment is `ORExt1819`. If you are not sure if you’ve specified this
correctly, you can check with `check_db()`. This function will also give
you a message about how to fix your code, if you formatted this
incorrectly.

``` r
check_db('1819', verbose = TRUE)
#> NOTE 1: User only provided digits 
#> 
#> NOTE 2: `db` argument must specify a 4-digit year, with the first two 
#> digits representing the start of the school year, and the 
#> last two digits representing the end of the school year. `db` may 
#> be passed with or without the `"ORExt"` prefix, e.g., `"1920"` 
#> or `"ORExt1920"`.
#> [1] "ORExt1819"
```

# Retreive table from database

Retreiving a given table from the database is simple. Calling
`db_get()`, we can access any of the following tables with the `table`
argument:

-   Accomodations
-   Answers
-   Districts
-   Exams
-   Items
-   Preferences
-   Schools
-   Students
-   Students_old
-   Submissions
-   SupplementalDistricts
-   SupplementalSchools
-   Tasks
-   User
-   UserStudents
-   UserStudents_old

The `table` argument is required and cannot be left blank. If you do not
specify the database (i.e., `db` in `db_get()`) it defaults to the
output of `db_current()`.

This is how you can access the Items table for the 2018-2019 academic
year, for example:

``` r
db_get(table = 'Items')
#> # A tibble: 960 × 4
#>    item_id standard item_id_brt item_difficulty   
#>    <chr>   <chr>    <chr>       <chr>             
#>  1 1       E03RF4   E03RF4L01   -2.45782391129564 
#>  2 2       E03RI2   E03RI2L03   -1.70952391129564 
#>  3 3       E03RL7   E03RL7L03   -1.61692391129564 
#>  4 4       E03WR4   E03WR4L03   -1.58992391129564 
#>  5 5       E03WR4   E03WR4L02   -1.32832391129564 
#>  6 6       E03WR1   E03WR1L01   -1.30912391129564 
#>  7 7       E03WR3   E03WR3M05   -1.0731157755135  
#>  8 8       E03WR4   E03WR4M06   -0.681423911295637
#>  9 9       E03RF4   E03RF4M07   -0.584923911295637
#> 10 10      E03WR4   E03WR4M07   -0.565123911295637
#> # … with 950 more rows
```

If we wanted to access the ids for various schools, we could do that
with the same function, but modifying `table`. Column names default to
`snake_case`. If you prefer `camelCase` names, you can access these with
`raw = TRUE`, indicating you want the unformatted (i.e., raw) column
names.

``` r
db_get(table = 'Schools', db = 'ORExt2021', raw = TRUE)
#> # A tibble: 1,708 × 3
#>    districtID schoolID name                      
#>         <int>    <int> <chr>                     
#>  1          0        0 ""                        
#>  2       1894        1 "Baker Middle School"     
#>  3       1894        2 "Brooklyn Primary"        
#>  4       1894        4 "Haines Elem"             
#>  5       1894        5 "Keating Elem"            
#>  6       1894        7 "South Baker Intermediate"
#>  7       1894        8 "Baker High"              
#>  8       1897       15 "Pine Eagle Charter"      
#>  9       1899       17 "Alsea Charter School"    
#> 10       1900       18 "Blodgett Elem"           
#> # … with 1,698 more rows
```

This package is meant to be used in conjunction with two other packages,
please see [`{exirt}`](https://github.com/UO-BRT/exirt) for an
explanation of the three-package pipeline.
