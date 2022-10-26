#' weighted 3-way crosstabs
#'
#' \code{crosstab_3way} returns a tibble containing a weighted crosstab of two variables by a third variable
#'
#'  Options  include row or cell percentages. The tibble can be in long or wide format.
#'  These tables are ideal for use with small multiples created with ggplot2::facet_wrap.
#'
#' @param df The data source
#' @param x The independent variable
#' @param y The dependent variable
#' @param z The second control variable
#' @param weight The weighting variable
#' @param remove An optional character vector of values to remove from final table (e.g. "refused").
#' This will not affect any calculations made. The vector is not case-sensitive.
#' @param n logical, if TRUE numeric totals are included.
#' @param pct_type Controls the kind of percentage values returned. One of "row" or "cell."
#' @param format one of "long" or "wide"
#' @param unwt_n logical, if TRUE a column is added containing unweighted frequency counts
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
#' crosstab_3way(df = illinois, x = sex, y = educ6, z = maritalstatus, weight = weight)
#' crosstab_3way(df = illinois, x = sex, y = educ6, z = maritalstatus, weight = weight,
#' format = "wide")

crosstab_3way <- function(df, x, y, z,
                          weight, remove = c(""),
                          n = TRUE, pct_type = "row", format = "wide",
                          unwt_n = FALSE){

  # make sure no weights are NA
  w <- df %>% pull({{weight}})
  if(length(w[is.na(w)]) > 0){
    stop("The weight variable contains missing values.", call. = FALSE)
  }

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
      mutate(total = sum({{weight}}),
             unweighted_n = n()) %>%
      # Calculate proportions
      group_by({{x}}, {{y}}, {{z}}) %>%
      summarise(pct = (sum({{weight}})/first(total))*100,
                n = first(total),
                unweighted_n = first(unweighted_n)) %>%
      # Remove values included in "remove" string
      filter(!str_to_upper({{x}}) %in% str_to_upper(remove),
             !str_to_upper({{y}}) %in% str_to_upper(remove),
             !str_to_upper({{z}}) %in% str_to_upper(remove)) %>%
      ungroup()

    # wide format, if required
    if(str_to_lower(format) == "wide"){
      d.output <- d.output %>%
        pivot_wider(names_from = {{y}}, values_from = pct,
                    values_fill = list(pct = 0), names_sort = TRUE) %>%
        select(-one_of("n", "unweighted_n"), one_of("n", "unweighted_n")) %>%
        arrange({{x}}, {{z}})
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
      mutate(total = sum({{weight}}),
             unweighted_n = n()) %>%
      # Calculate proportions
      group_by({{x}}, {{y}}, {{z}}) %>%
      summarise(pct = (sum({{weight}})/first(total))*100,
                n = first(total),
                unweighted_n = first(unweighted_n)) %>%
      # Remove values included in "remove" string
      filter(!str_to_upper({{x}}) %in% str_to_upper(remove),
             !str_to_upper({{y}}) %in% str_to_upper(remove),
             !str_to_upper({{z}}) %in% str_to_upper(remove)) %>%
      ungroup()

    # wide format, if required
    if(str_to_lower(format) == "wide"){
      d.output <- d.output %>%
        pivot_wider(names_from = {{y}}, values_from = pct,
                    values_fill = list(pct = 0), names_sort = TRUE) %>%
        select(-one_of("n", "unweighted_n"), one_of("n", "unweighted_n")) %>%
        arrange({{x}}, {{z}})
    }
  }


  if(n == FALSE){
    d.output <- select(d.output, -n)
  }

  if(unwt_n == FALSE){
    d.output <- select(d.output, -unweighted_n)
  }

  # test if date or number
  factor.true.type <- what_is_this_factor(pull(d.output, {{z}}))
  if(factor.true.type == "date"){
    d.output %>%
      as_tibble() %>%
      mutate({{z}} := as.Date({{z}}, tryFormats = c("%Y-%m-%d", "%Y/%m/%d","%d-%m-%Y","%m-%d-%Y")))
  } else if(factor.true.type == "number"){
    d.output %>%
      as_tibble() %>%
      mutate({{z}} := as.numeric(as.character({{z}})))
  } else{
    d.output %>%
      as_tibble()
  }
}
