#' weighted mean
#'
#' \code{wtd_mean} returns the weighted mean of a variable. It's a tidy-compatible
#' wrapper around stats::weighted.mean().
#'
#' @param df The data source
#' @param variable the variable, it should be numeric
#' @param weight The weighting variable
#'
#' @return a numeric value
#' @export
#' @import dplyr
#' @import rlang
#' @importFrom stats weighted.mean
#'
#' @examples
#' wtd_mean(illinois, age, weight)
#'
#' library(dplyr)
#' illinois %>% wtd_mean(age, weight)
wtd_mean <- function(df, variable, weight){
  df %>%
    summarise(mean = weighted.mean(x = {{variable}}, w = {{weight}})) %>%
    pull()
}

#' weighted summary table
#'
#' \code{summary_table} returns a tibble containing a weighted summary table of a single variable.
#'
#'  The resulting tible includes columns for the variable name, unweighted observations,
#'  weighted observations, weighted mean, minimum value, maximum value,
#'  unweighted missing values, and weighted missing values
#'
#' @param df The data source
#' @param variable the variable to summarize, it should be numeric
#' @param weight The weighting variable
#' @param name_style the style of the column names--one of "clean" or "pretty."
#' Clean names are all lower case and words are separated by an underscore.
#' Pretty names begin with a capital letter are words a separated by a space.
#'
#' @return a tibble
#' @export
#' @import dplyr
#' @import rlang
#' @importFrom stats weighted.mean
#'
#' @examples
#' summary_table(illinois, age, weight)
#' summary_table(illinois, age, weight, name_style = "pretty")
summary_table <- function(df, variable, weight, name_style = "clean"){

  # make sure no weights are NA
  w <- df %>% pull({{weight}})
  if(length(w[is.na(w)]) > 0){
    stop("The weight variable contains missing values.", call. = FALSE)
  }

  stopifnot(name_style %in% c("clean", "pretty"))

  unweighted_observations <- df %>%
    filter(!is.na({{variable}})) %>%
    pull({{variable}}) %>%
    length()

  weighted_observations <- df %>%
    filter(!is.na({{variable}})) %>%
    pull({{weight}}) %>%
    sum()

  weighted_mean <- df %>%
    wtd_mean({{variable}}, {{weight}})

  min_value <- df %>%
    summarise(min = min({{variable}}, na.rm = TRUE)) %>%
    pull()

  max_value <- df %>%
    summarise(max = max({{variable}}, na.rm = TRUE)) %>%
    pull()

  missing_observations <- df %>%
    filter(is.na({{variable}})) %>%
    pull({{variable}}) %>%
    length()

  missing_weighted_observations <- df %>%
    filter(is.na({{variable}})) %>%
    pull({{weight}}) %>%
    sum()

  variable_name <- df %>%
    select({{variable}}) %>%
    names()

  output <- tibble(variable_name, unweighted_observations, weighted_observations,
         weighted_mean, min_value, max_value, missing_observations,
         missing_weighted_observations)

  if(name_style == "pretty"){
    names(output) <- c("Variable", "Unweighted obs",
                       "Weighted obs", "Weighted mean", "Min", "Max",
                       "Unweighted missing", "Weighted missing")
  }

  output
}
