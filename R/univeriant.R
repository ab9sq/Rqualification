#' Univarent analysis of Lew data set.
#'
#' \code{lew} returns specifed univarent statistics.
#'
#' This function calulates specific univariant statistics from the NIST
#' Lew data set and returns a data frame for further evulation.
#'
#' @param na.rm A logical scaler. Should missing values be removed? depreciated
#'
#' @return A data frame that returns the mean and standard deviation calculated
#'     along with the NIST referenced values The data frame is in the same format
#'     as the data frame result for all NIST Univarent statistic in this package.
#'     \describe{
#'        \item{NIST}{The NIST data set utilized. for this function this value
#'        will be Lew}
#'        \item{mean}{The R calculated mean from the data set}
#'        \item{Standard_Deviation}{The R culculated standard deviation from the
#'        data set}
#'        \item{NIST_mead}{The NIST value of the mean for the data set, provided
#'        for reference}
#'        \item{NIST_Standard_Deviation}{Tne NIST value of of the standard deviation
#'        for the data set, provided for reference}
#'        }
#'
#'
#' @examples
#' Calculate.lew()
#'
#' @export
#'

Calculate.lew <- function() {
     options(digits = 15)
     utils::data("Lew")
     m <- mean(Lew$V1)
     s <- sd(Lew$V1)
     data.frame(
          NIST = "Lew",
          mean = m,
          Standard_Deviation = s,
          NIST_mean = -177.435000000000,
          NIST_Standard_Deviation = 277.332168044316
     )
}

#' Lew NIST Univarent Data Set
#'
#' INfo on data set
#'
#' @format 200 observation of 1 variable:
#'
#' @source \url{http://www.itl.nist.gov/div898/strd/univ/lew.html}
"Lew"
