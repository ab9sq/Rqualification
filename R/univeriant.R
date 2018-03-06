#' Univarent analysis of Lew data set.
#'
#' \code{calculate.lew} returns specifed univarent statistics.
#'
#' This function calulates specific univariant statistics from the NIST
#' Lew data set and returns a data frame for further evulation.
#'
#'
#'
#' @return A data frame that returns the mean and standard deviation calculated
#'     along with the NIST referenced values The data frame is in the same format
#'     as the data frame result for all NIST Univarent statistic in this dataset.
#'     \describe{
#'        \item{NIST}{The NIST data set utilized. For this function this value
#'        will be Lew}
#'        \item{mean}{The R calculated mean from the data set}
#'        \item{Standard_Deviation}{The R culculated standard deviation from the
#'        data set}
#'        \item{Autocorrelation_Coefficien}{Calculate population lag-1
#'        autocorrelation coefficient}
#'        \item{NIST_mean}{The NIST value of the mean for the data set, provided
#'        for reference}
#'        \item{NIST_Standard_Deviation}{Tne NIST value of of the standard deviation
#'        for the data set, provided for reference}
#'        \item{NIST_Autocorrelation_Coefficient}{Tne NIST value of of the population
#'        lag-1 autocorrelation coefficient}
#'        }
#'
#'
#' @examples
#' calculate.lew()
#'
#' @export
#'

calculate.lew <- function() {
     options(digits = 15)
     utils::data("Lew")
     m <- mean(Lew$V1)
     s <- sd(Lew$V1)
     auto <- acf(Lew$V1, plot=F, lag.max = 1)
     data.frame(
          NIST = "Lew",
          mean = m,
          Standard_Deviation = s,
          Autocorrelation_Coefficient = auto,
          NIST_mean = -177.435000000000,
          NIST_Standard_Deviation = 277.332168044316,
          NIST_Autocorrelation_Coefficient = -0.307304800605679
     )
}

#' Lew NIST Univarent Data Set.
#'
#' Dataset of 200 points
#'
#' This "real world" dataset is the result of a study by H.S. Lew of the Structures
#'     Division of the Cednter for Building Technology at the National Institute of
#'     Standards & Technology (NIST). The purpose of the study was to characterize
#'     the physical behavior of steel-concrete beams under periodic load. The rsponse
#'     variable is deflection (from the rest point) of the stel-concrete beam. the 200
#'     observations were collected equi-spaced in time.
#'
#'     \tabular{lr}{
#'     \strong{Level of Difficulty} \tab Lower\cr
#'     \strong{Variables} \tab 1\cr
#'     \strong{Observations} \tab 200\cr
#'     \strong{First observation} \tab -213\cr
#'     \strong{Expected results} (as certified)\tab \cr
#'     \strong{Mean} \tab -177.435000000000\cr
#'     \strong{Standard Deviation} \tab 277.332168044316\cr
#'     \strong{Population lag-1 autocorrelation coefficient} \tab -0.307304800605679\cr}

#'
#' @format 200 observation of 1 variable:
#'
#' @source \url{http://www.itl.nist.gov/div898/strd/univ/lew.html}
"Lew"


#NumAcc4

#' Univarent analysis of NumAcc4 data set.
#'
#' \code{calculate.NumAcc4} returns specifed univarent statistics.
#'
#' This function calulates specific univariant statistics from the NIST
#' NumAcc4 data set and returns a data frame for further evulation.
#'
#'
#' @return A data frame that returns the mean and standard deviation calculated
#'     along with the NIST referenced values The data frame is in the same format
#'     as the data frame result for all NIST Univarent statistic in this dataset.
#'     \describe{
#'        \item{NIST}{The NIST data set utilized. For this function this value
#'        will be NumAcc4}
#'        \item{mean}{The R calculated mean from the data set}
#'        \item{Standard_Deviation}{The R culculated standard deviation from the
#'        data set}
#'        \item{Autocorrelation_Coefficien}{Calculate population lag-1
#'        autocorrelation coefficient}
#'        \item{NIST_mean}{The NIST value of the mean for the data set, provided
#'        for reference}
#'        \item{NIST_Standard_Deviation}{Tne NIST value of of the standard deviation
#'        for the data set, provided for reference}
#'        \item{NIST_Autocorrelation_Coefficient}{Tne NIST value of of the population
#'        lag-1 autocorrelation coefficient}
#'        }

#'
#' @examples
#' calculate.NumAcc4()
#'
#' @export
#'

calculate.NumAcc4 <- function() {
     options(digits = 15)
     utils::data("NumAcc4")
     m <- mean(NumAcc4$V1)
     s <- sd(NumAcc4$V1)
     auto <- acf(NumAcc4$V1, plot=F, lag.max = 1)
     data.frame(
          NIST = "NumAcc4",
          mean = m,
          Standard_Deviation = s,
          Autocorrelation_Coefficient = auto,
          NIST_mean = 10000000.2,
          NIST_Standard_Deviation = 0.1,
          NIST_Autocorrelation_Coefficient = -0.999
     )
}

#' NumAcc4 NIST Univarent Data Set.
#'
#' Dataset of 1001 9-digit points
#'
#' This generated/fabricated dataset consists of 1001 9-digit floating-point
#'     values: a single 10000000.2, followed by 500 pairings of 10000000.1 and
#'     10000000.3. By construction, this data set has sample mean = 10000000.2
#'     (exact); sample standard deviation = .1 (exact); and sample autocorrelation
#'     coef. = -0.999 (exact). The construction was carried out based on
#'     considerations described by Simon, Stephen D. and Lesage, James P. (1989):
#'     Assessing the Accuracy of ANOVA Caluclations in Statistical Software",
#'     Computational Statistics \& data Analysis, 8, pp. 325-332.


#'
#'     \tabular{lr}{
#'     \strong{Level of Difficulty} \tab Higher\cr
#'     \strong{Variables} \tab 1\cr
#'     \strong{Observations} \tab 1001\cr
#'     \strong{First observation} \tab 100000000.2\cr
#'     \strong{Expected results} (as certified)\tab \cr
#'     \strong{Mean} \tab 10000000.2 (exact)\cr
#'     \strong{Standard Deviation} \tab 0.1 (exact)\cr
#'     \strong{Population lag-1 autocorrelation coefficient} \tab -0.999 (exact)\cr}

#'
#' @format 1001 observation of 1 variable:
#'
#' @source \url{http://www.itl.nist.gov/div898/strd/univ/numacc4.html}
"NumAcc4"

# NumAcc3

#' Univarent analysis of NumAcc3 data set.
#'
#' \code{calculate.NumAcc3} returns specifed univarent statistics.
#'
#' This function calulates specific univariant statistics from the NIST
#' NumAcc3 data set and returns a data frame for further evulation.
#'
#'
#' @return A data frame that returns the mean and standard deviation calculated
#'     along with the NIST referenced values The data frame is in the same format
#'     as the data frame result for all NIST Univarent statistic in this dataset.
#'     \describe{
#'        \item{NIST}{The NIST data set utilized. For this function this value
#'        will be NumAcc3}
#'        \item{mean}{The R calculated mean from the data set}
#'        \item{Standard_Deviation}{The R culculated standard deviation from the
#'        data set}
#'        \item{Autocorrelation_Coefficien}{Calculate population lag-1
#'        autocorrelation coefficient}
#'        \item{NIST_mean}{The NIST value of the mean for the data set, provided
#'        for reference}
#'        \item{NIST_Standard_Deviation}{Tne NIST value of of the standard deviation
#'        for the data set, provided for reference}
#'        \item{NIST_Autocorrelation_Coefficient}{Tne NIST value of of the population
#'        lag-1 autocorrelation coefficient}
#'        }

#'
#' @examples
#' calculate.NumAcc3()
#'
#' @export
#'

calculate.NumAcc3 <- function() {
     options(digits = 15)
     utils::data("NumAcc3")
     m <- mean(NumAcc3$V1)
     s <- sd(NumAcc3$V1)
     auto <- acf(NumAcc3$V1, plot=F, lag.max = 1)
     data.frame(
          NIST = "NumAcc3",
          mean = m,
          Standard_Deviation = s,
          Autocorrelation_Coefficient = auto,
          NIST_mean = 1000000.2,
          NIST_Standard_Deviation = 0.1,
          NIST_Autocorrelation_Coefficient = -0.999
     )
}

#' NumAcc3 NIST Univarent Data Set.
#'
#' Dataset of 1001 8-digit points
#'
#' This generated/fabricated dataset consists of 1001 8-digit floating-point
#'     values: a single 1000000.2, followed by 500 pairings of 1000000.1 and
#'     1000000.3. By construction, this data set has sample mean = 1000000.2
#'     (exact); sample standard deviation = .1 (exact); and sample autocorrelation
#'     coef. = -0.999 (exact). The construction was carried out based on
#'     considerations described by Simon, Stephen D. and Lesage, James P. (1989):
#'     Assessing the Accuracy of ANOVA Caluclations in Statistical Software",
#'     Computational Statistics \& data Analysis, 8, pp. 325-332.
#'
#'     \tabular{lr}{
#'     \strong{Level of Difficulty} \tab Average\cr
#'     \strong{Variables} \tab 1\cr
#'     \strong{Observations} \tab 1001\cr
#'     \strong{First observation} \tab 1000000.2\cr
#'     \strong{Expected results} (as certified)\tab \cr
#'     \strong{Mean} \tab 1000000.2 (exact)\cr
#'     \strong{Standard Deviation} \tab 0.1 (exact)\cr
#'     \strong{Population lag-1 autocorrelation coefficient} \tab -0.999 (exact)\cr}

#'
#' @format 1001 observation of 1 variable:
#'
#' @source \url{http://www.itl.nist.gov/div898/strd/univ/numacc3.html}
"NumAcc3"

# NumAcc2


#' Univarent analysis of NumAcc3 data set.
#'
#' \code{calculate.NumAcc2} returns specifed univarent statistics.
#'
#' This function calulates specific univariant statistics from the NIST
#' NumAcc2 data set and returns a data frame for further evulation.
#'
#'
#' @return A data frame that returns the mean and standard deviation calculated
#'     along with the NIST referenced values The data frame is in the same format
#'     as the data frame result for all NIST Univarent statistic in this dataset.
#'     \describe{
#'        \item{NIST}{The NIST data set utilized. For this function this value
#'        will be NumAcc2}
#'        \item{mean}{The R calculated mean from the data set}
#'        \item{Standard_Deviation}{The R culculated standard deviation from the
#'        data set}
#'        \item{Autocorrelation_Coefficien}{Calculate population lag-1
#'        autocorrelation coefficient}
#'        \item{NIST_mean}{The NIST value of the mean for the data set, provided
#'        for reference}
#'        \item{NIST_Standard_Deviation}{Tne NIST value of of the standard deviation
#'        for the data set, provided for reference}
#'        \item{NIST_Autocorrelation_Coefficient}{Tne NIST value of of the population
#'        lag-1 autocorrelation coefficient}
#'        }

#'
#' @examples
#' calculate.NumAcc2()
#'
#' @export
#'

calculate.NumAcc2 <- function() {
     options(digits = 15)
     utils::data("NumAcc2")
     m <- mean(NumAcc2$V1)
     s <- sd(NumAcc2$V1)
     auto <- acf(NumAcc2$V1, plot=F, lag.max = 1)
     data.frame(
          NIST = "NumAcc2",
          mean = m,
          Standard_Deviation = s,
          Autocorrelation_Coefficient = auto,
          NIST_mean = 1.2,
          NIST_Standard_Deviation = 0.1,
          NIST_Autocorrelation_Coefficient = -0.999
     )
}

#' NumAcc2 NIST Univarent Data Set.
#'
#' Dataset of 1001 2-digit points
#'
#' This generated/fabricated dataset consists of 1001 2-digit floating-point
#'      values: a single 1.2, followed by 500 pairings of 1.1 and 1.3. By
#'      construction, this data set has sample mean = 1.2 (exact); sample
#'      standard deviation = 0.1 (exact); and sample autocorrelation coef. = -0.999
#'      (exact). The construction was carried out based on considerations
#'      described by Simon, Stephen D. and Lesage, James P. (1989): Assessing
#'      the Accuracy of ANOVA Caluclations in Statistical Software", Computational
#'      Statistics \& data Analysis, 8, pp. 325-332.
#'
#'     \tabular{lr}{
#'     \strong{Level of Difficulty} \tab Average\cr
#'     \strong{Variables} \tab 1\cr
#'     \strong{Observations} \tab 1001\cr
#'     \strong{First observation} \tab 1.2\cr
#'     \strong{Expected results} (as certified)\tab \cr
#'     \strong{Mean} \tab 1.2 (exact)\cr
#'     \strong{Standard Deviation} \tab 0.1 (exact)\cr
#'     \strong{Population lag-1 autocorrelation coefficient} \tab -0.999 (exact)\cr}

#'
#' @format 101 observation of 1 variable:
#'
#' @source \url{http://www.itl.nist.gov/div898/strd/univ/numacc2.html}
"NumAcc2"

# NumAcc1


#' Univarent analysis of NumAcc3 data set.
#'
#' \code{calculate.NumAcc1} returns specifed univarent statistics.
#'
#' This function calulates specific univariant statistics from the NIST
#' NumAcc1 data set and returns a data frame for further evulation.
#'
#'
#' @return A data frame that returns the mean and standard deviation calculated
#'     along with the NIST referenced values The data frame is in the same format
#'     as the data frame result for all NIST Univarent statistic in this dataset.
#'     \describe{
#'        \item{NIST}{The NIST data set utilized. For this function this value
#'        will be NumAcc1}
#'        \item{mean}{The R calculated mean from the data set}
#'        \item{Standard_Deviation}{The R culculated standard deviation from the
#'        data set}
#'        \item{Autocorrelation_Coefficien}{Calculate population lag-1
#'        autocorrelation coefficient}
#'        \item{NIST_mean}{The NIST value of the mean for the data set, provided
#'        for reference}
#'        \item{NIST_Standard_Deviation}{Tne NIST value of of the standard deviation
#'        for the data set, provided for reference}
#'        \item{NIST_Autocorrelation_Coefficient}{Tne NIST value of of the population
#'        lag-1 autocorrelation coefficient}
#'        }

#'
#' @examples
#' calculate.NumAcc1()
#'
#' @export
#'

calculate.NumAcc1 <- function() {
     options(digits = 15)
     utils::data("NumAcc1")
     m <- mean(NumAcc1$V1)
     s <- sd(NumAcc1$V1)
     auto <- acf(NumAcc1$V1, plot=F, lag.max = 1)
     data.frame(
          NIST = "NumAcc1",
          mean = m,
          Standard_Deviation = s,
          Autocorrelation_Coefficient = auto,
          NIST_mean = 10000002,
          NIST_Standard_Deviation = 1,
          NIST_Autocorrelation_Coefficient = -0.5
     )
}

#' NumAcc1 NIST Univarent Data Set.
#'
#' Dataset of 3 8-digit points
#'
#' This generated/fabricated dataset consists of three 8-digit integers
#'     differing only in the least significant digit. The data set is: 10000002,
#'     10000001, and 10000003. By construction, this data set has sample
#'     mean = 10000002 (exact); sample standard deviation = 1 (exact); and
#'     sample autocorrelation coef. = -0.5 (exact). The construction was carried
#'     out based on considerations described by Simon, Stephen D. and Lesage,
#'     James P. (1989): Assessing the Accuracy of ANOVA Caluclations in Statistical
#'     Software", Computational Statistics \& data Analysis, 8, pp. 325-332
#'
#'     \tabular{lr}{
#'     \strong{Level of Difficulty} \tab Average\cr
#'     \strong{Variables} \tab 1\cr
#'     \strong{Observations} \tab 1001\cr
#'     \strong{First observation} \tab 10000001\cr
#'     \strong{Expected results} (as certified)\tab \cr
#'     \strong{Mean} \tab 10000002 (exact)\cr
#'     \strong{Standard Deviation} \tab 1 (exact)\cr
#'     \strong{Population lag-1 autocorrelation coefficient} \tab -0.5(exact)\cr}

#'
#' @format 3 observation of 1 variable:
#'
#' @source \url{http://www.itl.nist.gov/div898/strd/univ/NumAcc1.html}
"NumAcc1"

