#' weighted crosstabs
#'
#' \code{crosstab} returns a tibble containing a weighted crosstab of two variables
#'
#'  Options  include row, column, or cell percentages. The tibble can be in long or wide format.
#'
#' @param df The data source
#' @param x The independent variable
#' @param y The dependent variable
#' @param weight The weighting variable
#' @param remove An optional character vector of values to remove from final table (e.g. "refused").
#' This will not affect any calculations made. The vector is not case-sensitive.
#' @param n logical, if TRUE numeric totals are included. They are included in a separate column for row and cell
#' percentages, but in a separate row for wide format column percentages.
#' @param pct_type Controls the kind of percentage values returned. One of "row," "cell," or "column."
#' @param format one of "long" or "wide"
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
#' crosstab(df = illinois, x = voter, y = raceethnic, weight = weight)
#' crosstab(df = illinois, x = voter, y = raceethnic, weight = weight, n = FALSE)

crosstab <- function(df, x, y, weight, remove = "", n = TRUE, pct_type = "row", format = "wide"){

  # make sure the arguments are all correct
  stopifnot(pct_type %in% c("row", "cell", "column", "col"),
            format %in% c("wide", "long"))

  if(str_to_lower(pct_type) == "row"){
    d.output <- df %>%
      # Remove missing cases
      filter(!is.na({{x}}),
             !is.na({{y}})) %>%
      # Convert to ordered factors
      mutate({{x}} := to_factor({{x}}, sort_levels = "values"),
             {{y}} := to_factor({{y}}, sort_levels = "values")) %>%
      # Calculate denominator
      group_by({{x}}) %>%
      mutate(total = sum({{weight}})) %>%
      # Calculate proportions
      group_by({{x}}, {{y}}) %>%
      summarise(pct = (sum({{weight}})/first(total))*100,
                n = first(total)) %>%
      # Remove values included in "remove" string
      filter(!str_to_upper({{x}}) %in% str_to_upper(remove),
             !str_to_upper({{y}}) %in% str_to_upper(remove))

    # spread if format = wide
    if(format == "wide"){
      d.output <- d.output %>%
        # Spread so x is rows and y is columns
        pivot_wider(names_from = {{y}}, values_from = pct, values_fill = list(pct = 0)) %>%
        # move total row to end
        select(-one_of("n"), one_of("n")) %>%
        ungroup()
    }

    # remove n if required
    if(n == FALSE){
      d.output <- select(d.output, -n)
    }
  } else if(str_to_lower(pct_type) %in% c("col", "column")){
    d.output <- df %>%
      # Remove missing cases
      filter(!is.na({{x}}),
             !is.na({{y}})) %>%
      # Convert to ordered factors
      mutate({{x}} := to_factor({{x}}, sort_levels = "values"),
             {{y}} := to_factor({{y}}, sort_levels = "values")) %>%
      # calculate denominator
      group_by({{y}}) %>%
      mutate(total = sum({{weight}})) %>%
      # calculate proportions
      group_by({{x}}, {{y}}) %>%
      summarise(pct = (sum({{weight}})/first(total))*100,
                n = first(total)) %>%
      ungroup() %>%
      # remove values included in "remove" string
      filter(! str_to_upper({{x}}) %in% str_to_upper(remove),
             ! str_to_upper({{y}}) %in% str_to_upper(remove))

    if(format == "wide"){
      # make the total row separately
      total.row <- d.output %>%
        group_by({{y}}, n) %>%
        summarise() %>%
        pivot_wider(names_from = {{y}}, values_from = n, values_fill = list(pct = 0)) %>%
        mutate({{x}} := "n")

      # spread the output table
      d.output <- d.output %>%
        # drop the n column
        select(-n) %>%
        # spread so x is rows and y is columns
        pivot_wider(names_from = {{y}}, values_from = pct, values_fill = list(pct = 0))

      # if n = TRUE, then add then n row
      # this causes the response column to switch from factor to character
      if(n == TRUE){
        d.output <- suppressWarnings(bind_rows(d.output, total.row))
      }
    }

    # remove n column if n == FALSE
    if(n == FALSE){
      d.output <- select(d.output, -n)
    }
  } else if(str_to_lower(pct_type) == "cell"){
    d.output <- df %>%
      # Remove missing cases
      filter(!is.na({{x}}),
             !is.na({{y}})) %>%
      # Convert to ordered factors
      mutate({{x}} := to_factor({{x}}, sort_levels = "values"),
             {{y}} := to_factor({{y}}, sort_levels = "values")) %>%
      # Calculate denominator
      mutate(total = sum({{weight}})) %>%
      # Calculate proportions
      group_by({{x}}, {{y}}) %>%
      summarise(pct = (sum({{weight}})/first(total))*100,
                n = first(total)) %>%
      # Remove values included in "remove" string
      filter(!str_to_upper({{x}}) %in% str_to_upper(remove),
             !str_to_upper({{y}}) %in% str_to_upper(remove))

    # if format = wide, then spread the table
    if(format == "wide"){
      d.output <- d.output %>%
        # Spread so x is rows and y is columns
        pivot_wider(names_from = {{y}}, values_from = pct, values_fill = list(pct = 0)) %>%
        # move total row to end
        select(-one_of("n"), one_of("n")) %>%
        ungroup()
    }

    # remove n column if n == FALSE
    if(n == FALSE){
      d.output <- select(d.output, -n)
    }
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
