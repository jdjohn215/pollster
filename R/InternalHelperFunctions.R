# Thanks to Berry Boessenkool for this function
# https://github.com/brry/berryFunctions/blob/master/R/is.error.R
# This function checks if a function returns an error
is_error <- function(expr){
  expr_name <- deparse(substitute(expr))
  test <- try(expr, silent=TRUE)
  iserror <- inherits(test, "try-error")
  # output:
  iserror
}

# This function checks if a value is a date
is_date <- function(potentialdate){
  if(is_error(as.Date(potentialdate, tryFormats = c("%Y-%m-%d", "%Y/%m/%d","%d-%m-%Y","%m-%d-%Y"))) == TRUE){
    FALSE
  } else {TRUE}
}

# This function checks if a value is a number
is_a_number <- function(potentialnumber){
  !is.na(suppressWarnings(as.numeric(as.character(potentialnumber))))
}

# this function takes a vector of factor values & checks if all the
# values are (1) a date or (2) a number.
what_is_this_factor <- function(factorvector){
  if(all(sapply({{factorvector}}, is_date))){
    "date"
  } else if(all(sapply({{factorvector}}, is_a_number))) {
    "number"
  } else {
    "factor"
  }
}
