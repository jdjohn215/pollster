#' Calculate the design effect of a sample
#'
#' \code{deff_calc} returns a single number
#'
#'  This function returns the design effect of a given sample using the formula
#'  length(w)*sum(w^2)/(sum(w)^2).
#'  It is designed for use in the moe family of functions.
#'
#' @param w a vector of weights
#'
#' @return A number
#' @export
#'
#' @examples
#' deff_calc(illinois$weight)
#'
deff_calc <- function(w){
  length(w)*sum(w^2)/(sum(w)^2)
}


#' Calculate the margin of error (including design effect) of a sample
#'
#' \code{moedeff_calc} returns a single number. It is designed for use in the moe family of functions.
#'
#'  This function returns the margin of error including design effect of a given sample of weighted data using the formula
#'  sqrt(deff)*zscore*sqrt((pct*(1-pct))/(n-1))*100
#'
#' @param pct a proportion
#' @param deff a design effect
#' @param n the sample size
#' @param zscore defaults to 1.96, consistent with a 95\% confidence interval.
#'
#' @return A percentage
#' @export
#'
#' @examples
#' moedeff_calc(pct = 0.515, deff = 1.6, n = 214)
moedeff_calc <- function(pct, deff, n, zscore = 1.96){
  sqrt(deff)*zscore*sqrt((pct*(1-pct))/(n-1))*100
}
