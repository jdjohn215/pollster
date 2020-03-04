#' weighted topline with margin of error
#'
#' \code{moe_topline} returns a tibble containing a weighted topline of one variable with margin of error
#'
#'  By default the table includes a column for frequency count, percent, valid percent, and cumulative percent.
#'
#' @param variable the variable name
#' @param df The data source
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
#'
moe_topline <- function(variable, df, weight, remove = c(""),
                        n = TRUE, pct, valid_pct = TRUE, cum_pct = TRUE){

  # calculate the design effect
  deff <- df %>% pull({{weight}}) %>% deff_calc()

  # calculate the valid unweighted sample count
  unweighted.n <- df %>%
    filter(!is.na({{variable}})) %>%
    nrow()

  output <- df %>%
    filter(!is.na({{variable}})) %>%
    mutate({{variable}} := to_factor({{variable}}),
           total = sum({{weight}}),
           valid.total = sum(({{weight}})[{{variable}} != "(Missing)"])) %>%
    group_by({{variable}}) %>%
    summarise(valid.pct = (sum({{weight}})/first(valid.total)*100),
              n = sum({{weight}}),
              pct = (n/first(total))) %>%
    ungroup() %>%
    mutate(moe = moedeff_calc(pct = valid.pct/100, deff = deff, n = unweighted.n),
           cum = cumsum(valid.pct),
           valid.pct = replace(valid.pct, {{variable}} == "(Missing)", NA),
           cum = replace(cum, {{variable}} == "(Missing)", NA)) %>%
    mutate(pct = pct*100) %>%
    select(Response = {{variable}}, Frequency = n, Percent = pct,
           `Valid Percent` = valid.pct, `MOE` = moe, `Cumulative Percent` = cum) %>%
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

  output
}