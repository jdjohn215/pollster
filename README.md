`pollster` is an R package for making topline and crosstab tables of weighted survey data. The package is designed for use with labelled data, like what you might use the `haven` package to import from Stata or SPSS. It follows tidyverse programming conventions, and output tables are also in the form of a tidy data frame, or tibble.

The core functions are:

* `topline()`
* `crosstab()`
* `crosstab_3way()`

Each function also has a twin version which includes a column for the margin of error calculated to include the design effect of the weights.

* `moe_topline()`
* `moe_crosstab()`
* `moe_crosstab_3way()`

There are also two special functions which calculate the design effect for each survey wave independently.

* `moe_wave_crosstab()`
* `moe_wave_crosstab_3way()`
