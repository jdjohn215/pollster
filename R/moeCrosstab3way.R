#' weighted 3-way crosstabs with margin of error
#'
#' \code{moe_crosstab_3way} returns a tibble containing a weighted crosstab of two variables by a third variable with margin of error
#'
#'  Options  include row or cell percentages. The tibble can be in long or wide format.
#'  These tables are ideal for use with small multiples created with ggplot2::facet_wrap.
#'
#' @param x The independent variable
#' @param y The dependent variable
#' @param z The second control variable
#' @param df The data source
#' @param weight The weighting variable
#' @param remove An optional character vector of values to remove from final table (e.g. "refused").
#' This will not affect any calculations made. The vector is not case-sensitive.
#' @param n logical, if TRUE numeric totals are included.
#' @param pct_type Controls the kind of percentage values returned. One of "row" or "cell."
#' @param format one of "long" or "wide"
#'
#' @return a tibble
#' @export
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import labelled

moe_crosstab_3way <- function(x, y, z, df,
                              weight = zwave_weight, remove = c(""),
                              n = TRUE, pct_type = "row", format = "long"){
  # make sure the arguments are all correct
  stopifnot(pct_type %in% c("row", "cell"),
            format %in% c("wide", "long"))

  # calculate the design effect
  deff <- df %>% pull({{weight}}) %>% deff_calc()

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
      mutate(moe = moedeff_calc(pct = pct, deff = deff, n = n)) %>%
      mutate(pct = pct*100) %>%
      select(-observations) %>%
      # Remove values included in "remove" string
      filter(!str_to_upper({{x}}) %in% str_to_upper(remove),
             !str_to_upper({{y}}) %in% str_to_upper(remove),
             !str_to_upper({{z}}) %in% str_to_upper(remove)) %>%
      # move total row to end
      select(-one_of("n"), one_of("n"))
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
      mutate(moe = moedeff_calc(pct = pct, deff = deff, n = n)) %>%
      mutate(pct = pct*100) %>%
      select(-observations) %>%
      # Remove values included in "remove" string
      filter(!str_to_upper({{x}}) %in% str_to_upper(remove),
             !str_to_upper({{y}}) %in% str_to_upper(remove)) %>%
      # move total row to end
      select(-one_of("n"), one_of("n"))
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

  output
}