#' Checks For Correct Results of Univariate Tests.
#'
#' \code{check.univariate} runs each of the univivariate data sets and checks
#' to that the correct values are returned.
#'
#' This function runs each of the NIST univariate calculations against the
#' respective data set. It then compares the results to the given NIST values.
#' If the valuse do not agree within the expected degree of machine accuracy,
#' the data set and statistic are logged.
#'
#' Information on machine accuracy can be found at
#' \url{http://www.validlab.com/goldberg/paper.pdf} and was calculated with
#' \code{.Machine$double.ebs ^ 0.5}.
#'
#'
#' @return The function returns a vector that shows the version of R the test
#' was conducted on, the data and time the test was started, the machine
#' accuracy utilized, the specific data sets run, any noted errors and a count of
#' errors encountered.
#'
#'
#' @examples
#' check.univariate()
#'
#' a <- check.univariate()
#'
#' @export
#'
check.univariate <- function() {
   #setup
   options(digits = 22)
   machine.error <- .Machine$double.eps ^ 0.5
   version.tested <- paste("R Version tested:",
                           R.Version()$version.string,
                           R.Version()$nickname)
   date.tested <- paste("Date of test:",
                        date())
   results <- NULL
   results <- c(version.tested, date.tested)
   results <- c(results,
                " ",
                paste("degree of accuracy (or machine accuracy) ",
                      machine.error),
                " ")
   # testing function
   check <- function(test.results, test.set) {
      working <- NULL
      # check mean
      if (!(test.results$mean == test.results$NIST_mean)) {
         if (abs(test.results$mean - test.results$NIST_mean)
             > machine.error) {
            error.message <- paste(" ",
                                   "ERROR ERROR ERROR",
                                   "Mean calculation failed in",
                                   test.set)
            working <- c(working,
                         error.message)
            difference <- (test.results$mean - test.results$NIST_mean)
            error.message <- paste("     The difference is:",
                                   difference)
            working <- c(working,
                         error.message)
            relative.error <- (difference / test.results$NIST_mean) * 100
            error.message <- paste("     The relative error is:",
                                    relative.error,
                                    "%")
            working <- c(working,
                         error.message,
                         "ERROR ERROR ERROR",
                         "  ")
         } else {
            error.message <- paste(
               "wARNING WARNING WARNING",
               "Mean calculation not an exact match but within machine accuracy for",
               test.set
            )
            working <- c(working,
                         error.message)
            difference <- (test.results$mean - test.results$NIST_mean)
            error.message <- paste("      The difference is:",
                                   difference)
            working <- c(working,
                         error.message)
            relative.error <- (difference / test.results$NIST_mean) * 100
            error.message <- paste("     The relative error is:",
                                    relative.error,
                                    "%")
            working <- c(working,
                         error.message,
                         "  ")

         }
      } else {
         error.message <- paste("Mean calculation agreed for",
                                test.set)
         working <- c(working,
                      error.message,
                      " ")
      }
      # check standard deviation
      if (!(test.results$Standard_Deviation ==
            test.results$NIST_Standard_Deviation)) {
         if ( (
            abs(
               test.results$Standard_Deviation -
               test.results$NIST_Standard_Deviation
            )
         )
         > machine.error) {
            error.message <- paste(
               " ",
               "ERROR ERROR ERROR",
               "Standard Deviation calculation failed in",
               test.set,
               "ERROR ERROR ERROR",
               "  "
            )
            working <- c(working,
                         error.message)
            difference <- test.results$Standard_Deviation -
                  test.results$NIST_Standard_Deviation
            error.message <- paste("    The difference is:",
                                   difference)
            working <- c(working,
                         error.message)
            relative.error <- (difference / test.results$NIST_Standard_Deviation) * 100
            error.message <- paste("     The relative error is:",
                                    relative.error,
                                    "%")
            working <- c(working,
                         error.message,
                         "ERROR ERROR ERROR",
                         "  ")

         } else {
            error.message <- paste(
               "WARNING WARNING warning",
               "Standard Deviation calculation not an exact match but within machine accuracy for",
               test.set
            )
            working <- c(working,
                         error.message)
            difference <- test.results$Standard_Deviation -
                  test.results$NIST_Standard_Deviation
            error.message <- paste("      The difference is:",
                                   difference)
            working <- c(working,
                         error.message)
            relative.error <- (difference / test.results$NIST_Standard_Deviation) * 100
            error.message <- paste("     The relative error is:",
                                    relative.error,
                                    "%")
            working <- c(working,
                         error.message,
                         "  ")
         }
      } else {
         error.message <- paste("Standard Deviation calculation agreed for",
                                test.set)
         working <- c(working,
                      error.message,
                      " ")
      }
      # Check autocorrelation
      if (!(
         test.results$Autocorrelation_Coefficient ==
         test.results$NIST_Autocorrelation_Coefficient
      )) {
         if (abs(
            test.results$Autocorrelation_Coefficient -
            test.results$NIST_Autocorrelation_Coefficient
         )
         > machine.error) {
            error.message <- paste(
               " ",
               "ERROR ERROR ERROR",
               "Autocorrelation coefficien calculation failed in",
               test.set,
               "ERROR ERROR ERROR",
               "  "
            )
            working <- c(working,
                         error.message)
            difference <- test.results$Autocorrelation_Coefficient -
                     test.results$NIST_Autocorrelation_Coefficient
            error.message <- paste("      The difference is; ",
                                   difference)
            working <- c(working,
                         error.message,
                         "  ")
            relative.error <- (difference / test.results$NIST_Autocorrelation_Coefficient) * 100
            error.message <- paste("     The relative error is:",
                                    relative.error,
                                    "%")
            working <- c(working,
                         error.message,
                         "ERROR ERROR ERROR",
                         "  ")
         } else {
            error.message <- paste(
               "WARNING WARNING WARNING",
               "Autocorrelation coefficient calculation not an exact match but within machine accuracy for",
               test.set
            )
            working <- c(working,
                         error.message)
            difference <- test.results$Autocorrelation_Coefficient -
                     test.results$NIST_Autocorrelation_Coefficient
            error.message <- paste("     The difference is:",
                                   difference)
            working <- c(working,
                         error.message)
            relative.error <- (difference / test.results$NIST_Autocorrelation_Coefficient) * 100
            error.message <- paste("     The relative error is:",
                                    relative.error,
                                    "%")
            working <- c(working,
                         error.message,
                         "  ")
         }
      } else {
         error.message <-
            paste("Autocorrelation coefficient calculation agreed for",
                  test.set)
         working <- c(working,
                      error.message,
                      " ")
      }
      return(working)
   }

   # >>>>>>>>>>>>>>>>>> start tests <<<<<<<<<<<<<<<<<<<<

   # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Lew
   test.set <- "Lew"
   results <- c(results,
                paste("Running", test.set, "data set"))
   test.results <- calculate.lew()
   results.hold <-
      check(test.results = test.results, test.set = test.set)
   results <- c(results,
                results.hold,
                " ")

   # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> NumAcc4
   test.set <- "NumAcc4"
   results <- c(results,
                paste("Running", test.set, "data set"))
   test.results <- calculate.NumAcc4()
   results.hold <-
      check(test.results = test.results, test.set = test.set)
   results <- c(results,
                results.hold,
                " ")

   # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> NumAcc3
   test.set <- "NumAcc3"
   results <- c(results,
                paste("Running", test.set, "data set"))
   test.results <- calculate.NumAcc3()
   results.hold <-
      check(test.results = test.results, test.set = test.set)
   results <- c(results,
                results.hold,
                " ")

   # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> NumAcc2
   test.set <- "NumAcc2"
   results <- c(results,
                paste("Running", test.set, "data set"))
   test.results <- calculate.NumAcc2()
   results.hold <-
      check(test.results = test.results, test.set = test.set)
   results <- c(results,
                results.hold,
                " ")

   # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> NumAcc1
   test.set <- "NumAcc1"
   results <- c(results,
                paste("Running", test.set, "data set"))
   test.results <- calculate.NumAcc1()

   results.hold <-
      check(test.results = test.results, test.set = test.set)
   results <- c(results,
                results.hold,
                " ")

   # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Michelso
   test.set <- "Michelso"
   results <- c(results,
                paste("Running", test.set, "data set"))
   test.results <- calculate.michelso()

   results.hold <-
      check(test.results = test.results, test.set = test.set)
   results <- c(results,
                results.hold,
                " ")

   # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Mavro
   test.set <- "Mavro"
   results <- c(results,
                paste("Running", test.set, "data set"))
   test.results <- calculate.mavro()

   results.hold <-
      check(test.results = test.results, test.set = test.set)
   results <- c(results,
                results.hold,
                " ")

   # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Lottery
   test.set <- "Lottery"
   results <- c(results,
                paste("Running", test.set, "data set"))
   test.results <- calculate.lottery()

   results.hold <-
      check(test.results = test.results, test.set = test.set)
   results <- c(results,
                results.hold,
                " ")

   # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Pi Digits
   test.set <- "Pi Digits"
   results <- c(results,
                paste("Running", test.set, "data set"))
   test.results <- calculate.PiDigits()

   results.hold <-
      check(test.results = test.results, test.set = test.set)
   results <- c(results,
                results.hold,
                " ")
   return(results)
}
