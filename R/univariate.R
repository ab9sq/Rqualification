#' Univariate analysis of Lew data set.
#'
#'
#' \code{calculate.lew} returns specifed univariate statistics.
#'
#' This function calulates specific Univariate statistics from the NIST
#' Lew data set and returns a data frame for further evulation.
#'
#'
#'
#' @return A data frame that returns the mean, standard deviation and population
#'     lag-1 autocorrelation coefficient calculated
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
#' calculate.lew()
#' a <- calculate.lew()
#'
#' @export
#'

calculate.lew <- function() {
     options(digits = 15)
     m <- base::mean(RQualification::Lew$V1)
     s <- stats::sd(RQualification::Lew$V1)
     auto <- stats::acf(RQualification::Lew$V1, plot = F, lag.max = 1)
     auto <- unlist(auto)
     auto <- as.numeric(auto[2])
     data.frame(
          NIST = "Lew",
          mean = m,
          Standard_Deviation = s,
          Autocorrelation_Coefficient = auto,
          NIST_mean = -177.435000000000,
          NIST_Standard_Deviation = 277.332168044316,
          NIST_Autocorrelation_Coefficient = -0.307304800605679,
          stringsAsFactors = FALSE
     )
}


#' Univariate analysis of NumAcc4 data set.
#'
#' \code{calculate.NumAcc4} returns specifed univarent statistics.
#'
#' This function calulates specific univariate statistics from the NIST
#' NumAcc4 data set and returns a data frame for further evulation.
#'
#'
#' @return A data frame that returns the mean, standard deviation and population
#'     lag-1 autocorrelation coefficient calculated
#'     along with the NIST referenced values The data frame is in the same format
#'     as the data frame result for all NIST Univarent statistic in this dataset.
#'     \itemize{
#'        \item{\strong{NIST} The NIST data set utilized. For this function this value
#'        will be NumAcc4}
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
#' @examples
#' calculate.NumAcc4()
#' a <- calculate.NumAcc4()
#'
#' @export
#'

calculate.NumAcc4 <- function() {
     options(digits = 15)
     m <- base::mean(RQualification::NumAcc4$V1)
     s <- stats::sd(RQualification::NumAcc4$V1)
     auto <- stats::acf(RQualification::NumAcc4$V1, plot = F, lag.max = 1)
     auto <- unlist(auto)
     auto <- as.numeric(auto[2])
     data.frame(
          NIST = "NumAcc4",
          mean = m,
          Standard_Deviation = s,
          Autocorrelation_Coefficient = auto,
          NIST_mean = 10000000.2,
          NIST_Standard_Deviation = 0.1,
          NIST_Autocorrelation_Coefficient = -0.999,
          stringsAsFactors = FALSE
     )
}


#' Univariate analysis of NumAcc3 data set.
#'
#' \code{calculate.NumAcc3} returns specifed univarent statistics.
#'
#' This function calulates specific Univariate statistics from the NIST
#' NumAcc3 data set and returns a data frame for further evulation.
#'
#'
#' @return A data frame that returns the mean, standard deviation and population
#'     lag-1 autocorrelation coefficient calculated
#'     along with the NIST referenced values The data frame is in the same format
#'     as the data frame result for all NIST Univarent statistic in this dataset.
#'     \itemize{
#'        \item{\strong{NIST} The NIST data set utilized. For this function this value
#'        will be NumAcc3}
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
#' @examples
#' calculate.NumAcc3()
#' a <- calculate.NumAcc3()
#'
#' @export
#'

calculate.NumAcc3 <- function() {
     options(digits = 15)
     m <- base::mean(RQualification::NumAcc3$V1)
     s <- stats::sd(RQualification::NumAcc3$V1)
     auto <- stats::acf(RQualification::NumAcc3$V1, plot = F, lag.max = 1)
     auto <- unlist(auto)
     auto <- as.numeric(auto[2])
     data.frame(
          NIST = "NumAcc3",
          mean = m,
          Standard_Deviation = s,
          Autocorrelation_Coefficient = auto,
          NIST_mean = 1000000.2,
          NIST_Standard_Deviation = 0.1,
          NIST_Autocorrelation_Coefficient = -0.999,
          stringsAsFactors = FALSE
     )
}


#' Univariate analysis of NumAcc2 data set.
#'
#' \code{calculate.NumAcc2} returns specifed univarent statistics.
#'
#' This function calulates specific Univariate statistics from the NIST
#' NumAcc2 data set and returns a data frame for further evulation.
#'
#'
#' @return A data frame that returns the mean, standard deviation and population
#'     lag-1 autocorrelation coefficient calculated
#'     along with the NIST referenced values The data frame is in the same format
#'     as the data frame result for all NIST Univarent statistic in this dataset.
#'     \itemize{
#'        \item{\strong{NIST} The NIST data set utilized. For this function this value
#'        will be NumAcc2}
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
#' @examples
#' calculate.NumAcc2()
#' a <- calculate.NumAcc2()
#'
#' @export
#'

calculate.NumAcc2 <- function() {
     options(digits = 15)
     m <- base::mean(RQualification::NumAcc2$V1)
     s <- stats::sd(RQualification::NumAcc2$V1)
     auto <- stats::acf(RQualification::NumAcc2$V1, plot = F, lag.max = 1)
     auto <- unlist(auto)
     auto <- as.numeric(auto[2])
     data.frame(
          NIST = "NumAcc2",
          mean = m,
          Standard_Deviation = s,
          Autocorrelation_Coefficient = auto,
          NIST_mean = 1.2,
          NIST_Standard_Deviation = 0.1,
          NIST_Autocorrelation_Coefficient = -0.999,
          stringsAsFactors = FALSE
     )
}


#' Univariate analysis of NumAcc1 data set.
#'
#' \code{calculate.NumAcc1} returns specifed Univariate statistics.
#'
#' This function calulates specific Univariate statistics from the NIST
#' NumAcc1 data set and returns a data frame for further evulation.
#'
#'
#' @return A data frame that returns the mean, standard deviation and population
#'     lag-1 autocorrelation coefficient calculated
#'     along with the NIST referenced values The data frame is in the same format
#'     as the data frame result for all NIST Univarent statistic in this dataset.
#'     \itemize{
#'        \item{\strong{NIST} The NIST data set utilized. For this function this value
#'        will be NumAcc1}
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
#' @examples
#' calculate.NumAcc1()
#' a <- calculate.NumAcc1()
#'
#' @export
#'

calculate.NumAcc1 <- function() {
     options(digits = 15)
     m <- base::mean(RQualification::NumAcc1$V1)
     s <- stats::sd(RQualification::NumAcc1$V1)
     auto <- stats::acf(RQualification::NumAcc1$V1, plot = F, lag.max = 1)
     auto <- unlist(auto)
     auto <- as.numeric(auto[2])
     data.frame(
          NIST = "NumAcc1",
          mean = m,
          Standard_Deviation = s,
          Autocorrelation_Coefficient = auto,
          NIST_mean = 10000002,
          NIST_Standard_Deviation = 1,
          NIST_Autocorrelation_Coefficient = -0.5,
          stringsAsFactors = FALSE
     )
}


#' Univariate analysis of Michelso data set.
#'
#' \code{calculate.Michelso} returns specifed Univariate statistics.
#'
#' This function calulates specific Univariate statistics from the NIST
#' Michelso data set and returns a data frame for further evulation.
#'
#'
#' @return A data frame that returns the mean, standard deviation and population
#'     lag-1 autocorrelation coefficient calculated
#'     along with the NIST referenced values The data frame is in the same format
#'     as the data frame result for all NIST Univarent statistic in this dataset.
#'     \itemize{
#'        \item{\strong{NIST} The NIST data set utilized. For this function this value
#'        will be Michelso}
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
#' @examples
#' calculate.michelso()
#' a <- calculate.michelso()
#'
#' @export
#'

calculate.michelso <- function() {
     options(digits = 15)
     m <- base::mean(RQualification::Michelso$V1)
     s <- stats::sd(RQualification::Michelso$V1)
     auto <- stats::acf(RQualification::Michelso$V1, plot = F, lag.max = 1)
     auto <- unlist(auto)
     auto <- as.numeric(auto[2])
     data.frame(
          NIST = "Michelso",
          mean = m,
          Standard_Deviation = s,
          Autocorrelation_Coefficient = auto,
          NIST_mean = 299.852400000000,
          NIST_Standard_Deviation = 0.0790105478190518,
          NIST_Autocorrelation_Coefficient = 0.535199668621283,
          stringsAsFactors = FALSE
     )
}


#' Univariate analysis of Mavro data set.
#'
#' \code{calculate.Mavro} returns specifed Univariate statistics.
#'
#' This function calulates specific Univariate statistics from the NIST
#' Mavro data set and returns a data frame for further evulation.
#'
#'
#' @return A data frame that returns the mean, standard deviation and population
#'     lag-1 autocorrelation coefficient calculated
#'     along with the NIST referenced values The data frame is in the same format
#'     as the data frame result for all NIST Univarent statistic in this dataset.
#'     \itemize{
#'        \item{\strong{NIST} The NIST data set utilized. For this function this value
#'        will be Mavro}
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
#' @examples
#' calculate.mavro()
#' a <- calculate.mavro()
#' @export
#'

calculate.mavro <- function() {
     options(digits = 15)
     m <- base::mean(RQualification::Mavro$V1)
     s <- stats::sd(RQualification::Mavro$V1)
     auto <- stats::acf(RQualification::Mavro$V1, plot = F, lag.max = 1)
     auto <- unlist(auto)
     auto <- as.numeric(auto[2])
     data.frame(
          NIST = "Mavro",
          mean = m,
          Standard_Deviation = s,
          Autocorrelation_Coefficient = auto,
          NIST_mean = 2.00185600000000,
          NIST_Standard_Deviation = 0.000429123454003053,
          NIST_Autocorrelation_Coefficient = 0.937989183438248,
          stringsAsFactors = FALSE
     )
}


#' Univariate analysis of Lottery data set.
#'
#' \code{calculate.Lottery} returns specifed Univariate statistics.
#'
#' This function calulates specific Univariate statistics from the NIST
#' Lottery data set and returns a data frame for further evulation.
#'
#'
#' @return A data frame that returns the mean, standard deviation and population
#'     lag-1 autocorrelation coefficient calculated
#'     along with the NIST referenced values The data frame is in the same format
#'     as the data frame result for all NIST Univarent statistic in this dataset.
#'     \itemize{
#'        \item{\strong{NIST} The NIST data set utilized. For this function this value
#'        will be Lottery}
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
#' @examples
#' calculate.lottery()
#' a <- calculate.lottery()
#' @export
#'

calculate.lottery <- function() {
     options(digits = 15)
     m <- base::mean(RQualification::Lottery$V1)
     s <- stats::sd(RQualification::Lottery$V1)
     auto <- stats::acf(RQualification::Lottery$V1, plot = F, lag.max = 1)
     auto <- unlist(auto)
     auto <- as.numeric(auto[2])
     data.frame(
          NIST = "Lottery",
          mean = m,
          Standard_Deviation = s,
          Autocorrelation_Coefficient = auto,
          NIST_mean = 518.958715596330,
          NIST_Standard_Deviation = 291.699727470969,
          NIST_Autocorrelation_Coefficient = -0.120948622967393,
          stringsAsFactors = FALSE
     )
}

#' Univariate analysis of Pi Digits data set.
#'
#' \code{calculate.PiDigits} returns specifed Univariate statistics.
#'
#' This function calulates specific Univariate statistics from the NIST
#' Pi Digits data set and returns a data frame for further evulation.
#'
#'
#' @return A data frame that returns the mean, standard deviation and population
#'     lag-1 autocorrelation coefficient calculated
#'     along with the NIST referenced values The data frame is in the same format
#'     as the data frame result for all NIST Univarent statistic in this dataset.
#'     \itemize{
#'        \item{\strong{NIST} The NIST data set utilized. For this function this value
#'        will be PiDigits}
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
#' @examples
#' calculate.PiDigits()
#' a <- calculate.PiDigits()
#' @export
#'

calculate.PiDigits <- function() {
     options(digits = 15)
     m <- base::mean(RQualification::PiDigits$V1)
     s <- stats::sd(RQualification::PiDigits$V1)
     auto <- stats::acf(RQualification::PiDigits$V1, plot = F, lag.max = 1)
     auto <- unlist(auto)
     auto <- as.numeric(auto[2])
     data.frame(
          NIST = "PiDigits",
          mean = m,
          Standard_Deviation = s,
          Autocorrelation_Coefficient = auto,
          NIST_mean = 4.53480000000000,
          NIST_Standard_Deviation = 2.86733906028871,
          NIST_Autocorrelation_Coefficient = -0.00355099287237972,
          stringsAsFactors = FALSE
     )
}
