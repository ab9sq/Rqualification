#' Linear regression analysis of Norris data set.
#'
#' \code{calculate.lew} returns specifed univariate statistics.
#'
#' This function calulates specific Univariate statistics from the NIST
#' Lew data set and returns a data frame for further evulation.
#'
#'
#'
#' @return A data frame that returns the calculated intercept, slope, R-Squared,
#'     F statisitic, sum of the squares, mean square
#'     along with the NIST referenced values The data frame is in the same format
#'     as the data frame result for all NIST Univarent statistic in this dataset.
#'     \itemize{
#'        \item{\strong{NIST} The NIST data set utilized. For this function this value
#'        will be Lew}
#'        \item{\strong{mean} The R calculated mean from the data set}
#'        \item{\strong{Standard_Deviation} The R culculated standard deviation from the
#'        data set}
#'        \item{\strong{Autocorrelation_Coefficien} Calculate population lag-1
#'        autocorrelation coefficient}
#'        \item{\strong{NIST_mean} The NIST value of the mean for the data set, provided
#'        for reference}
#'        \item{\strong{NIST_Standard_Deviation} The NIST value of of the standard deviation
#'        for the data set, provided for reference}
#'        \item{\strong{NIST_Autocorrelation_Coefficient} The NIST value of of the population
#'        lag-1 autocorrelation coefficient}
#'        }
#'
#'
#' @examples
#' calculate.norris()
#' a <- calculate.norris()
#'
#' @export
#'

calculate.norris <- function() {
   options(digits = 22)
   Norris.lm <- stats::lm(y ~ x, data = RQualification::Norris)
   intercept <- Norris.lm$coefficients[1]
   intercept <- unlist(intercept)
   intercept <- as.numeric(intercept)

   data.frame(
      NIST = "Norris",
      intercept = intercept,
      NIST_intercept = -0.262323073774029,
      stringsAsFactors = FALSE
   )
}
