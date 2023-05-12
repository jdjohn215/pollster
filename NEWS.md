# pollster 0.1.6
* fix bug which lead to explicitly missing factor levels still being included in the "valid percent" column.

# pollster 0.1.5
* replace deprecated `forcats::fct_explicit_na` with `forcats::fct_na_value_to_level`
* require at least forcats v1.0.0

# pollster 0.1.4
* rows are explicitly rearranged by x and z in wide 3-way crosstabs
* weights of 0 are removed prior to calculation of the design effect
* an error is now given in all table-creating functions if the weight variable includes NA values

# pollster 0.1.3
* crosstab functions now include an option to include a column with unweighted frequencies. currently it is not available for column percents.
* a bug is fixed that gave an error in `crosstab(..., pct_type = "col", format = "wide", n = FALSE)`
* `crosstab_3way` now places the n column at the end of the dataframe, consistent with `crosstab`
* fix bug in `moe_crosstab` & `moe_crosstab_3way` that reported unweighted n in place of weighted n

# pollster 0.1.2

* pollster now depends on the most recent version of tidyr (1.1.0) because it uses the argument `names_sort = TRUE` to ensure that `tidyr::pivot_wider` arranges rows and columns in the order of their factor levels.

# pollster 0.1.1

* improvements to how crosstab functions conditionally convert factor to date class. This includes removing the lubridate dependency.
* crosstab functions now convert factors in crosstabs to numeric values when all values are numeric
* crosstabs now show a value of 0% instead of NA when there are no values.
* add CRAN installation to readme


# pollster 0.1.0

* package accepted by CRAN on 03/25/2020
