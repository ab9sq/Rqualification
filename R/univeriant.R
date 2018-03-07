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
     m <- base::mean(RQualification::Lew$V1)
     s <- stats::sd(RQualification::Lew$V1)
     auto <- stats::acf(RQualification::Lew$V1, plot = F, lag.max = 1)
     auto <- unlist(auto)
     auto <- auto[2]
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
     m <- base::mean(RQualification::NumAcc4$V1)
     s <- stats::sd(RQualification::NumAcc4$V1)
     auto <- stats::acf(RQualification::NumAcc4$V1, plot = F, lag.max = 1)
     auto <- unlist(auto)
     auto <- auto[2]
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
     m <- base::mean(RQualification::NumAcc3$V1)
     s <- stats::sd(RQualification::NumAcc3$V1)
     auto <- stats::acf(RQualification::NumAcc3$V1, plot = F, lag.max = 1)
     auto <- unlist(auto)
     auto <- auto[2]
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




#' Univarent analysis of NumAcc2 data set.
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
#' calculate.NumAcc3()
#'
#' @export
#'

calculate.NumAcc2 <- function() {
     options(digits = 15)
     m <- base::mean(RQualification::NumAcc2$V1)
     s <- stats::sd(RQualification::NumAcc2$V1)
     auto <- stats::acf(RQualification::NumAcc2$V1, plot = F, lag.max = 1)
     auto <- unlist(auto)
     auto <- auto[2]
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




#' Univarent analysis of NumAcc1 data set.
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
     m <- base::mean(RQualification::NumAcc1$V1)
     s <- stats::sd(RQualification::NumAcc1$V1)
     auto <- stats::acf(RQualification::NumAcc1$V1, plot = F, lag.max = 1)
     auto <- unlist(auto)
     auto <- auto[2]
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





#' Univarent analysis of Michelso data set.
#'
#' \code{calculate.Michelso} returns specifed univarent statistics.
#'
#' This function calulates specific univariant statistics from the NIST
#' Michelso data set and returns a data frame for further evulation.
#'
#'
#' @return A data frame that returns the mean and standard deviation calculated
#'     along with the NIST referenced values The data frame is in the same format
#'     as the data frame result for all NIST Univarent statistic in this dataset.
#'     \describe{
#'        \item{NIST}{The NIST data set utilized. For this function this value
#'        will be Michelso}
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
#' calculate.michelso()
#'
#' @export
#'

calculate.michelso <- function() {
     options(digits = 15)
     m <- base::mean(RQualification::Michelso$V1)
     s <- stats::sd(RQualification::Michelso$V1)
     auto <- stats::acf(RQualification::Michelso$V1, plot = F, lag.max = 1)
     auto <- unlist(auto)
     auto <- auto[2]
     data.frame(
          NIST = "Michelso",
          mean = m,
          Standard_Deviation = s,
          Autocorrelation_Coefficient = auto,
          NIST_mean = 299.852400000000,
          NIST_Standard_Deviation = 0.0790105478190518,
          NIST_Autocorrelation_Coefficient = 0.535199668621283
     )
}

#' Michelso NIST Univarent Data Set.
#'
#' Dataset of 100 points
#'
#' This "real world" dataset is the result of the classic study conducted by
#'     Michelson on the speed of light in air in 1879. The response variable is
#'     speed of light (in millions of meters per second). The data was included
#'     as part of a larger study by Dorsey, Ernest N. (1944) on the velocity of
#'     light as reported in the Transactions of the American Philosophical Society.
#'
#'     \tabular{lr}{
#'     \strong{Level of Difficulty} \tab Lower\cr
#'     \strong{Variables} \tab 1\cr
#'     \strong{Observations} \tab 100\cr
#'     \strong{First observation} \tab 299.85\cr
#'     \strong{Expected results} (as certified)\tab \cr
#'     \strong{Mean} \tab 299.852400000000\cr
#'     \strong{Standard Deviation} \tab 0.0790105478190518\cr
#'     \strong{Population lag-1 autocorrelation coefficient} \tab 0.535199668621283\cr}

#'
#' @format 100 observation of 1 variable:
#'
#' @source \url{http://www.itl.nist.gov/div898/strd/univ/Michelso.html}
"Michelso"




#' Univarent analysis of Mavro data set.
#'
#' \code{calculate.Mavro} returns specifed univarent statistics.
#'
#' This function calulates specific univariant statistics from the NIST
#' Mavro data set and returns a data frame for further evulation.
#'
#'
#' @return A data frame that returns the mean and standard deviation calculated
#'     along with the NIST referenced values The data frame is in the same format
#'     as the data frame result for all NIST Univarent statistic in this dataset.
#'     \describe{
#'        \item{NIST}{The NIST data set utilized. For this function this value
#'        will be Mavro}
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
#' calculate.mavro()
#'
#' @export
#'

calculate.mavro <- function() {
     options(digits = 15)
     m <- base::mean(RQualification::Mavro$V1)
     s <- stats::sd(RQualification::Mavro$V1)
     auto <- stats::acf(RQualification::Mavro$V1, plot = F, lag.max = 1)
     auto <- unlist(auto)
     auto <- auto[2]
     data.frame(
          NIST = "Mavro",
          mean = m,
          Standard_Deviation = s,
          Autocorrelation_Coefficient = auto,
          NIST_mean = 2.00185600000000,
          NIST_Standard_Deviation = 0.000429123454003053,
          NIST_Autocorrelation_Coefficient = 0.937989183438248
     )
}

#' Mavro NIST Univarent Data Set.
#'
#' Dataset of 50 points
#'
#' This "real world" dataset is the result of a study by Radu Mavrodineaunu, a
#'     chemist at the National Institute of Standards & Technology (NIST). The
#'     purpose of the study was to determine a certified transmittance value
#'     that may be attached to the particular of filter under study. The 50
#'     transmittance valuess were collected equi-spaced in time at a sampling
#'     rate of 10 observations per second.
#'
#'     \tabular{lr}{
#'     \strong{Level of Difficulty} \tab Lower\cr
#'     \strong{Variables} \tab 1\cr
#'     \strong{Observations} \tab 50\cr
#'     \strong{First observation} \tab 2.00180\cr
#'     \strong{Expected results} (as certified)\tab \cr
#'     \strong{Mean} \tab 2.00185600000000\cr
#'     \strong{Standard Deviation} \tab 0.000429123454003053\cr
#'     \strong{Population lag-1 autocorrelation coefficient} \tab 0.937989183438248\cr}

#'
#' @format 50 observation of 1 variable:
#'
#' @source \url{http://www.itl.nist.gov/div898/strd/univ/Mavro.html}
"Mavro"




#' Univarent analysis of Lottery data set.
#'
#' \code{calculate.Lottery} returns specifed univarent statistics.
#'
#' This function calulates specific univariant statistics from the NIST
#' Lottery data set and returns a data frame for further evulation.
#'
#'
#' @return A data frame that returns the mean and standard deviation calculated
#'     along with the NIST referenced values The data frame is in the same format
#'     as the data frame result for all NIST Univarent statistic in this dataset.
#'     \describe{
#'        \item{NIST}{The NIST data set utilized. For this function this value
#'        will be Lottery}
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
#' calculate.lottery()
#'
#' @export
#'

calculate.lottery <- function() {
     options(digits = 15)
     m <- base::mean(RQualification::Lottery$V1)
     s <- stats::sd(RQualification::Lottery$V1)
     auto <- stats::acf(RQualification::Lottery$V1, plot = F, lag.max = 1)
     auto <- unlist(auto)
     auto <- auto[2]
     data.frame(
          NIST = "Lottery",
          mean = m,
          Standard_Deviation = s,
          Autocorrelation_Coefficient = auto,
          NIST_mean = 518.958715596330,
          NIST_Standard_Deviation = 291.699727470969,
          NIST_Autocorrelation_Coefficient = -0.120948622967393
     )
}

#' Lottery NIST Univarent Data Set.
#'
#' Dataset of 218 3-digit points
#'
#' This dataset consists of 218 3-digit numbers (from 000 to 999) resulting from
#'     the state of Maryland's Pick-3 Lottery. The data was collected for the
#'     32-week period September 3, 1989 to April 14, 1990. One 3-digit random
#'     number was drawn per day, 7 days per week for most weeks, but 6 or 5 days
#'     per week for other weeks. Interesting data-analytic questions involving
#'     the dataset are 1) are the lottery numbers uniformly distributed? and 2)
#'     is there serial correlation between lottery numbers?
#'
#'
#'     \tabular{lr}{
#'     \strong{Level of Difficulty} \tab Lower\cr
#'     \strong{Variables} \tab 1\cr
#'     \strong{Observations} \tab 218\cr
#'     \strong{First observation} \tab 162\cr
#'     \strong{Expected results} (as certified)\tab \cr
#'     \strong{Mean} \tab 518.958715596330\cr
#'     \strong{Standard Deviation} \tab 291.699727470969\cr
#'     \strong{Population lag-1 autocorrelation coefficient} \tab -0.120948622967393\cr}

#'
#' @format 218 observation of 1 variable:
#'
#' @source \url{http://www.itl.nist.gov/div898/strd/univ/lottery.html}
"Lottery"




#' Univarent analysis of Pi Digits data set.
#'
#' \code{calculate.PiDigits} returns specifed univarent statistics.
#'
#' This function calulates specific univariant statistics from the NIST
#' Pi Digits data set and returns a data frame for further evulation.
#'
#'
#' @return A data frame that returns the mean and standard deviation calculated
#'     along with the NIST referenced values The data frame is in the same format
#'     as the data frame result for all NIST Univarent statistic in this dataset.
#'     \describe{
#'        \item{NIST}{The NIST data set utilized. For this function this value
#'        will be PiDigits}
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
#' calculate.PiDigits()
#'
#' @export
#'

calculate.PiDigits <- function() {
     options(digits = 15)
     m <- base::mean(RQualification::PiDigits$V1)
     s <- stats::sd(RQualification::PiDigits$V1)
     auto <- stats::acf(RQualification::PiDigits$V1, plot = F, lag.max = 1)
     auto <- unlist(auto)
     auto <- auto[2]
     data.frame(
          NIST = "PiDigits",
          mean = m,
          Standard_Deviation = s,
          Autocorrelation_Coefficient = auto,
          NIST_mean = 4.53480000000000,
          NIST_Standard_Deviation = 2.86733906028871,
          NIST_Autocorrelation_Coefficient = -0.00355099287237972
     )
}

#' Pi Digits NIST Univarent Data Set.
#'
#' Dataset of 5000 points
#'
#' This dataset consists of the first 5000 digits of the mathemtatical constant
#'     pi (= 3.1415926535897932384...). These 5000 digits were reported in
#'     Mathematics of Computation, January 1962, page 76. Interesting
#'     number-theoretic questions involving pi digits are 1) are the digits
#'     uniformly distributed? and 2) is there serial correlation between
#'     successive digits?
#'
#'     \tabular{lr}{
#'     \strong{Level of Difficulty} \tab Lower\cr
#'     \strong{Variables} \tab 1\cr
#'     \strong{Observations} \tab 5000\cr
#'     \strong{First observation} \tab 3\cr
#'     \strong{Expected results} (as certified)\tab \cr
#'     \strong{Mean} \tab 4.53480000000000\cr
#'     \strong{Standard Deviation} \tab 2.86733906028871\cr
#'     \strong{Population lag-1 autocorrelation coefficient} \tab -0.00355099287237972\cr}

#'
#' @format 218 observation of 1 variable:
#'
#' @source \url{http://www.itl.nist.gov/div898/strd/univ/pidigits.html}
"PiDigits"
