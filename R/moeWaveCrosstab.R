#' weighted crosstabs with margin of error, where the x-variable identifies different survey waves
#'
#' \code{moe_wave_crosstab} returns a tibble containing a weighted crosstab of two variables
#'  with margin of error. Use this function when the x-variable indicates different survey
#'  waves for which weights were calculated independently.
#'
#'  Options  include row or cell percentages. The tibble can be in long or wide format. The margin of
#'  error includes the design effect of the weights, calculated separately for each
#'  survey wave.
#'
#' @param df The data source
#' @param x The independent variable, which uniquely identifies survey waves
#' @param y The dependent variable
#' @param weight The weighting variable, defaults to zwave_weight
#' @param remove An optional character vector of values to remove from final table (e.g. "refused").
#' This will not affect any calculations made. The vector is not case-sensitive.
#' @param n logical, if TRUE numeric totals are included.
#' @param pct_type Controls the kind of percentage values returned. One of "row" or "cell."
#' Column percents are not supported.
#' @param format one of "long" or "wide"
#' @param zscore defaults to 1.96, consistent with a 95\% confidence interval
#' @param unwt_n logical, if TRUE it adds a column with unweighted frequency values
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
#' moe_wave_crosstab(df = illinois, x = year, y = maritalstatus, weight = weight)
#' moe_wave_crosstab(df = illinois, x = year, y = maritalstatus, weight = weight, format = "wide")

moe_wave_crosstab <- function(df, x, y, weight, remove = c(""), n = TRUE,
                              pct_type = "row", format = "long",
                              zscore = 1.96, unwt_n = FALSE){

  # make sure no weights are NA
  w <- df %>% pull({{weight}})
  if(length(w[is.na(w)]) > 0){
    stop("The weight variable contains missing values.", call. = FALSE)
  }

  # make sure the arguments are all correct
  stopifnot(pct_type %in% c("row", "cell"),
            format %in% c("wide", "long"))

  # Calculate the design effect for each wave individually, as that is
  # the level at which the weights are calculated
  stats.by.wave <- df %>%
    filter(!is.na({{x}})) %>%
    mutate({{x}} := to_factor({{x}})) %>%
    group_by({{x}}) %>%
    summarise(deff = deff_calc({{weight}})) %>%
    ungroup()

  # build the table, either row percents or cell percents
  if(pct_type == "row"){
    d.output <- df %>%
      filter(!is.na({{x}}),
             !is.na({{y}})) %>%
      mutate({{x}} := to_factor({{x}}),
             {{y}} := to_factor({{y}})) %>%
      group_by({{x}}) %>%
      mutate(total = sum({{weight}}),
             unweighted_n = length({{weight}})) %>%
      group_by({{x}}, {{y}}) %>%
      summarise(observations = sum({{weight}}),
                pct = observations/first(total),
                n = first(total),
                unweighted_n = first(unweighted_n)) %>%
      ungroup() %>%
      inner_join(stats.by.wave) %>%
      mutate(moe = moedeff_calc(pct = pct, deff = deff, n = unweighted_n, zscore = zscore)) %>%
      mutate(pct = pct*100) %>%
      select(-observations) %>%
      # Remove values included in "remove" string
      filter(!str_to_upper({{x}}) %in% str_to_upper(remove),
             !str_to_upper({{y}}) %in% str_to_upper(remove)) %>%
      # move total row to end
      select(-one_of("n", "unweighted_n"), one_of("n", "unweighted_n")) %>%
      select(-deff)
  } else if(pct_type == "cell"){
    d.output <- df %>%
      filter(!is.na({{x}}),
             !is.na({{y}})) %>%
      mutate({{x}} := to_factor({{x}}),
             {{y}} := to_factor({{y}})) %>%
      # calculate denominator
      mutate(total = sum({{weight}}),
             unweighted_n = length({{weight}})) %>%
      group_by({{x}}, {{y}}) %>%
      summarise(observations = sum({{weight}}),
                pct = observations/first(total),
                n = first(total),
                unweighted_n = first(unweighted_n)) %>%
      ungroup() %>%
      inner_join(stats.by.wave) %>%
      mutate(moe = moedeff_calc(pct = pct, deff = deff, n = unweighted_n, zscore = zscore)) %>%
      mutate(pct = pct*100) %>%
      select(-observations) %>%
      # Remove values included in "remove" string
      filter(!str_to_upper({{x}}) %in% str_to_upper(remove),
             !str_to_upper({{y}}) %in% str_to_upper(remove)) %>%
      # move total row to end
      select(-one_of("n", "unweighted_n"), one_of("n", "unweighted_n")) %>%
      select(-deff)
  }

  # convert to wide format if required
  if(format == "wide"){
    d.output <- d.output %>%
      pivot_wider(names_from = {{y}}, values_from = c(pct, moe),
                  values_fill = list(pct = 0, moe = 0)) %>%
      select(-one_of("n", "unweighted_n"), one_of("n", "unweighted_n"))
  }

  # remove n if required
  if(n == FALSE){
    d.output <- select(d.output, -n)
  }
  # remove unweighted_n if required
  if(unwt_n == FALSE){
    d.output <- select(d.output, -unweighted_n)
  }

  # test if date or number
  factor.true.type <- what_is_this_factor(pull(d.output, {{x}}))
  if(factor.true.type == "date"){
    d.output %>%
      as_tibble() %>%
      mutate({{x}} := as.Date({{x}}, tryFormats = c("%Y-%m-%d", "%Y/%m/%d","%d-%m-%Y","%m-%d-%Y")))
  } else if(factor.true.type == "number"){
    d.output %>%
      as_tibble() %>%
      mutate({{x}} := as.numeric(as.character({{x}})))
  } else{
    d.output %>%
      as_tibble()
  }
}
