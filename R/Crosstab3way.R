#' weighted 3-way crosstabs
#'
#' \code{crosstab.3way} returns a tibble containing a weighted crosstab of two variables
#'
#'  Options  include row, column, or cell percentages. The tibble can be in long or wide format.
#'  These tables are ideal for use with small multiples created with ggplot2::facet_wrap.
#'
#' @param x The independent variable
#' @param y The dependent variable
#' @param z The grouping variable
#' @param df The data source
#' @param weight The weighting variable, defaults to zwave_weight
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

crosstab.3way <- function(x, y, z, df,
                          weight = zwave_weight, remove = c(""),
                          n = TRUE, pct_type = "row", format = "wide"){

  # make sure the arguments are all correct
  stopifnot(pct_type %in% c("row", "cell"),
            format %in% c("wide", "long"))

  # row percents
  if(pct_type == "row"){
    d.output <- df %>%
      # Remove missing cases
      filter(!is.na({{x}}),
             !is.na({{y}}),
             !is.na({{z}})) %>%
      # Convert to ordered factors
      mutate({{x}} := to_factor({{x}}, sort_levels = "values"),
             {{y}} := to_factor({{y}}, sort_levels = "values"),
             {{z}} := to_factor({{z}}, sort_levels = "values")) %>%
      # Calculate denominator
      group_by({{x}}, {{z}}) %>%
      mutate(total = sum({{weight}})) %>%
      # Calculate proportions
      group_by({{x}}, {{y}}, {{z}}) %>%
      summarise(pct = (sum({{weight}})/first(total))*100,
                n = first(total)) %>%
      # Remove values included in "remove" string
      filter(!str_to_upper({{x}}) %in% str_to_upper(remove),
             !str_to_upper({{y}}) %in% str_to_upper(remove),
             !str_to_upper({{z}}) %in% str_to_upper(remove)) %>%
      ungroup()

    # wide format, if required
    if(str_to_lower(format) == "wide"){
      d.output <- d.output %>%
        pivot_wider(names_from = {{y}}, values_from = pct,
                    values_fill = list(pct = 0))
    }
  } else if(pct_type == "cell"){
    d.output <- df %>%
      # Remove missing cases
      filter(!is.na({{x}}),
             !is.na({{y}}),
             !is.na({{z}})) %>%
      # Convert to ordered factors
      mutate({{x}} := to_factor({{x}}, sort_levels = "values"),
             {{y}} := to_factor({{y}}, sort_levels = "values"),
             {{z}} := to_factor({{z}}, sort_levels = "values")) %>%
      # Calculate denominator
      group_by({{z}}) %>%
      mutate(total = sum({{weight}})) %>%
      # Calculate proportions
      group_by({{x}}, {{y}}, {{z}}) %>%
      summarise(pct = (sum({{weight}})/first(total))*100,
                n = first(total)) %>%
      # Remove values included in "remove" string
      filter(!str_to_upper({{x}}) %in% str_to_upper(remove),
             !str_to_upper({{y}}) %in% str_to_upper(remove),
             !str_to_upper({{z}}) %in% str_to_upper(remove)) %>%
      ungroup()

    # wide format, if required
    if(str_to_lower(format) == "wide"){
      d.output <- d.output %>%
        pivot_wider(names_from = {{y}}, values_from = pct,
                    values_fill = list(pct = 0))
    }
  }


  if(n == FALSE){
    d.output <- select(d.output, -n)
  }

  d.output %>% as_tibble()
}