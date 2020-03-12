#' weighted topline
#'
#' \code{topline} returns a tibble containing a weighted topline of one variable
#'
#'  By default the table includes a column for frequency count, percent, valid percent, and cumulative percent.
#'
#' @param df The data source
#' @param variable the variable name
#' @param weight The weighting variable, defaults to zwave_weight
#' @param remove An optional character vector of values to remove from final table (e.g. "refused").
#' This will not affect any calculations made. The vector is not case-sensitive.
#' @param n logical, if TRUE a frequency column is included
#' percentages, but in a separate row for column percentages.
#' @param pct logical, if TRUE a column of percents is included
#' @param valid_pct logical, if TRUE a column of valid percents is included
#' @param cum_pct logical, if TRUE a column of cumulative percents is included
#'
#' @return a tibble
#' @export
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import labelled
#' @import rlang
#'
#' @examples
#' topline(illinois, sex, weight)
#' topline(illinois, sex, weight, pct = FALSE)

topline <- function(df, variable, weight, remove = c(""), n = TRUE,
                    pct = TRUE, valid_pct = TRUE, cum_pct = TRUE){
  # Make table
  d.output <- df %>%
    # Convert to ordered factors
    mutate({{variable}} := to_factor({{variable}}, sort_levels = "values"),
           {{variable}} := forcats::fct_explicit_na({{variable}})) %>%
    # Calculate denominator
    mutate(total = sum({{weight}}),
           valid.total = sum(({{weight}})[{{variable}} != "(Missing)"])) %>%
    # Calculate proportions
    group_by({{variable}}) %>%
    summarise(pct = (sum({{weight}})/first(total))*100,
              valid.pct = (sum({{weight}})/first(valid.total)*100),
              n = sum({{weight}})) %>%
    ungroup() %>%
    mutate(cum = cumsum(valid.pct),
           valid.pct = replace(valid.pct, {{variable}} == "(Missing)", NA),
           cum = replace(cum, {{variable}} == "(Missing)", NA)) %>%
    select(Response = {{variable}}, Frequency = n, Percent = pct,
           `Valid Percent` = valid.pct, `Cumulative Percent` = cum) %>%
    # Remove values included in "remove" string
    filter(! str_to_upper(Response) %in% str_to_upper(remove))

  # remove columns as requested
  if(valid_pct == FALSE){
    d.output <- select(d.output, -`Valid Percent`)
  }

  if(cum_pct == FALSE){
    d.output <- select(d.output, -`Cumulative Percent`)
  }

  if(n == FALSE){
    d.output <- select(d.output, -Frequency)
  }

  if(pct == FALSE){
    d.output <- select(d.output, -Percent)
  }

  d.output %>%
    as_tibble()
}
