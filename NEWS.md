# pollster 0.1.1.9000

* due to new updates in tidyr ([discussed here](https://github.com/tidyverse/tidyr/issues/839)), `pivot_wider` no longer arranges columns according to their factor levels. The development version of tidyr contains a fix to this bug, but until then the pollster functions `crosstab` and `crosstab_3way` are reverting back to `tidyr::spread`. Because the functions `moe_crosstab` and `moe_crosstab_3way` work by passing two columns to the `values_from` argument (a feature unavailable in `spread`) they cannot be reverted. Thus in some cases `crosstab` and `moe_crosstab` may return tibbles with different column orders. This behavior will be fixed as soon as tidyr is updated.

# pollster 0.1.1

* improvements to how crosstab functions conditionally convert factor to date class. This includes removing the lubridate dependency.
* crosstab functions now convert factors in crosstabs to numeric values when all values are numeric
* crosstabs now show a value of 0% instead of NA when there are no values.
* add CRAN installation to readme


# pollster 0.1.0

* package accepted by CRAN on 03/25/2020
