#' weighted 3-way crosstabs with margin of error, where the z-variable identifies different survey waves
#'
#' \code{moe_wave_crosstab_3way} returns a tibble containing a weighted crosstab of two variables by a third variable with margin of error.
#' Use this function when the z-variable indicates different survey
#' waves for which weights were calculated independently.
#'
#'  Options  include row or cell percentages. The tibble can be in long or wide format.
#'  These tables are ideal for use with small multiples created with ggplot2::facet_wrap.
#'
#' @param df The data source
#' @param x The independent variable
#' @param y The dependent variable
#' @param z The second control variable, uniquely identifies survey waves
#' @param weight The weighting variable
#' @param remove An optional character vector of values to remove from final table (e.g. "refused").
#' This will not affect any calculations made. The vector is not case-sensitive.
#' @param n logical, if TRUE numeric totals are included.
#' @param pct_type Controls the kind of percentage values returned. One of "row" or "cell."
#' @param format one of "long" or "wide"
#' @param zscore defaults to 1.96, consistent with a 95\% confidence interval
#'
#' @return a tibble
#' @export
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import labelled
#' @import rlang
#' @importFrom lubridate as_date
#'
#' @examples
#' moe_crosstab_3way(df = illinois, x = sex, y = educ6, z = year, weight = weight)
#' moe_crosstab_3way(df = illinois, x = sex, y = educ6, z = year, weight = weight, format = "wide")

moe_wave_crosstab_3way <- function(df, x, y, z,
                              weight, remove = c(""),
                              n = TRUE, pct_type = "row", format = "long",
                              zscore = 1.96){
  # make sure the arguments are all correct
  stopifnot(pct_type %in% c("row", "cell"),
            format %in% c("wide", "long"))

  # Calculate the design effect for each wave individually, as that is
  # the level at which the weights are calculated
  stats.by.wave <- df %>%
    filter(!is.na({{z}})) %>%
    mutate({{z}} := to_factor({{z}})) %>%
    group_by({{z}}) %>%
    summarise(deff = deff_calc({{weight}})) %>%
    ungroup()

  # build the table, either row percents or cell percents
  if(pct_type == "row"){
    output <- df %>%
      filter(!is.na({{x}}),
             !is.na({{y}}),
             !is.na({{z}})) %>%
      mutate({{x}} := to_factor({{x}}),
             {{y}} := to_factor({{y}}),
             {{z}} := to_factor({{z}})) %>%
      group_by({{z}}, {{x}}) %>%
      mutate(total = sum({{weight}}),
             n = length({{weight}})) %>%
      group_by({{z}}, {{x}}, {{y}}) %>%
      summarise(observations = sum({{weight}}),
                pct = observations/first(total),
                n = first(n)) %>%
      ungroup() %>%
      inner_join(stats.by.wave) %>%
      mutate(moe = moedeff_calc(pct = pct, deff = deff, n = n, zscore = zscore)) %>%
      mutate(pct = pct*100) %>%
      select(-observations) %>%
      # Remove values included in "remove" string
      filter(!str_to_upper({{x}}) %in% str_to_upper(remove),
             !str_to_upper({{y}}) %in% str_to_upper(remove),
             !str_to_upper({{z}}) %in% str_to_upper(remove)) %>%
      # move total row to end
      select(-one_of("n"), one_of("n")) %>%
      select(-deff)
  } else if(pct_type == "cell"){
    output <- df %>%
      filter(!is.na({{x}}),
             !is.na({{y}})) %>%
      mutate({{x}} := to_factor({{x}}),
             {{y}} := to_factor({{y}})) %>%
      # calculate denominator
      group_by({{z}}) %>%
      mutate(total = sum({{weight}}),
             n = length({{weight}})) %>%
      group_by({{z}}, {{x}}, {{y}}) %>%
      summarise(observations = sum({{weight}}),
                pct = observations/first(total),
                n = first(n)) %>%
      ungroup() %>%
      inner_join(stats.by.wave) %>%
      mutate(moe = moedeff_calc(pct = pct, deff = deff, n = n, zscore = zscore)) %>%
      mutate(pct = pct*100) %>%
      select(-observations) %>%
      # Remove values included in "remove" string
      filter(!str_to_upper({{x}}) %in% str_to_upper(remove),
             !str_to_upper({{y}}) %in% str_to_upper(remove)) %>%
      # move total row to end
      select(-one_of("n"), one_of("n")) %>%
      select(-deff)
  }

  # convert to wide format if required
  if(format == "wide"){
    output <- output %>%
      pivot_wider(names_from = {{y}}, values_from = c(pct, moe))
  }

  # remove n if required
  if(n == FALSE){
    output <- select(output, -n)
  }

  # test if date
  is.it.a.date <- is_date(df %>% pull({{z}}))

  if(is.it.a.date == TRUE){
    output %>%
      as_tibble() %>%
      mutate({{z}} := lubridate::as_date({{z}}))
  } else{
    output %>%
      as_tibble()
  }
}
