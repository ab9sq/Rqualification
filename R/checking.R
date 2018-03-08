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
#'
#' @param none
#'
#' @return The function returns a vector that shows the version of R the test
#' was conducted on, the data and time the test was started, the machine
#' accuracy utilized, the specific data sets run, any noted errors and a count of
#' errors encountered.
#'
#' Information of machine accuracy can be found at
#' \url{http://www.validlab.com/goldberg/paper.pdf} and was calculated with
#' \code{.Machine$double.ebs ^ 0.5}.
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
     options(digits = 15)
     machine.error <- .Machine$double.eps ^ 0.5
     version.tested <- paste("R Version tested:",
                             R.Version()$version.string,
                             R.Version()$nickname,
                             sep = " ")
     date.tested <- paste("Date of test:",
                          date(),
                          sep = " ")
     results <- NULL
     results <- c(version.tested, date.tested)
     results <- c(results," ",
                  paste("degree of accuracy ",
                        machine.error),
                  " ")
     error.count <- 0

     # -------------------------------------------------------      Lew
     test.set <- "Lew"
     results <- c(results,
                  paste("running", test.set))
     test.results <- calculate.lew()
     if (!(test.results$mean == test.results$NIST_mean)) {
          if ((abs(test.results$mean - test.results$NIST_mean))
               > machine.error) {
               error.count <- error.count + 1
               error.message <- paste("Mean calculation failed in",
                                      test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }
     if (!(test.results$Standard_Deviation ==
           test.results$NIST_Standard_Deviation)) {
          if ((
               abs(test.results$Standard_Deviation -
                   test.results$NIST_Standard_Deviation)
          ) > machine.error) {
               error.count <- error.count + 1
               error.message <-
                    paste("Standard deviation calculation failed in",
                          test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }
     if (!(test.results$Autocorrelation_Coefficient ==
           test.results$NIST_Autocorrelation_Coefficient)) {
          if (abs(test.results$Autocorrelation_Coefficient -
                   test.results$NIST_Autocorrelation_Coefficient) >
               machine.error) {
               error.count <- error.count + 1
               error.message <-
                    paste("Autocorrelation coefficient calculation failed in",
                          test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }


     # -------------------------------------------------------      NumAcc4
     test.set <- "NumAcc4"
     results <- c(results,
                  paste("running", test.set))
     test.results <- calculate.NumAcc4()
     if (!(test.results$mean == test.results$NIST_mean)) {
          if ((abs(test.results$mean - test.results$NIST_mean)) >
              machine.error) {
               error.count <- error.count + 1
               error.message <- paste("Mean calculation failed in",
                                      test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }
     if (!(test.results$Standard_Deviation ==
           test.results$NIST_Standard_Deviation)) {
          if (abs(test.results$Standard_Deviation -
                   test.results$NIST_Standard_Deviation)
           > machine.error) {
               error.count <- error.count + 1
               error.message <-
                    paste("Standard deviation calculation failed in",
                          test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }
     if (!(test.results$Autocorrelation_Coefficient ==
           test.results$NIST_Autocorrelation_Coefficient)) {
          if (abs(test.results$Autocorrelation_Coefficient -
                  test.results$NIST_Autocorrelation_Coefficient)
          > machine.error) {
               error.count <- error.count + 1
               error.message <-
                    paste("Autocorrelation coefficient calculation failed in",
                          test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }

     # -------------------------------------------------------      NumAcc3
     test.set <- "NumAcc3"
     results <- c(results,
                  paste("running", test.set))
     test.results <- calculate.NumAcc3()
     if (!(test.results$mean == test.results$NIST_mean)) {
          if (abs(test.results$mean - test.results$NIST_mean)
              > machine.error) {
               error.count <- error.count + 1
               error.message <- paste("Mean calculation failed in",
                                      test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }
     if (!(test.results$Standard_Deviation ==
           test.results$NIST_Standard_Deviation)) {
          if (abs(test.results$Standard_Deviation -
                  test.results$NIST_Standard_Deviation)
          > machine.error) {
               error.count <- error.count + 1
               error.message <-
                    paste("Standard deviation calculation failed in",
                          test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }
     if (!(test.results$Autocorrelation_Coefficient ==
           test.results$NIST_Autocorrelation_Coefficient)) {
          if (abs(test.results$Autocorrelation_Coefficient -
                   test.results$NIST_Autocorrelation_Coefficient)
          > machine.error) {
               error.count <- error.count + 1
               error.message <-
                    paste("Autocorrelation coefficient calculation failed in",
                          test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }

     # -------------------------------------------------------      NumAcc2
     test.set <- "NumAcc2"
     results <- c(results,
                  paste("running", test.set))
     test.results <- calculate.NumAcc2()
     if (!(test.results$mean == test.results$NIST_mean)) {
          if (abs(test.results$mean - test.results$NIST_mean)
              > machine.error) {
               error.count <- error.count + 1
               error.message <- paste("Mean calculation failed in",
                                      test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }
     if (!(test.results$Standard_Deviation ==
           test.results$NIST_Standard_Deviation)) {
          if (abs(test.results$Standard_Deviation -
                  test.results$NIST_Standard_Deviation)
              > machine.error) {
               error.count <- error.count + 1
               error.message <-
                    paste("Standard deviation calculation failed in",
                          test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }
     if (!(test.results$Autocorrelation_Coefficient ==
           test.results$NIST_Autocorrelation_Coefficient)) {
          if (abs(test.results$Autocorrelation_Coefficient -
                  test.results$NIST_Autocorrelation_Coefficient)
              > machine.error) {
               error.count <- error.count + 1
               error.message <-
                    paste("Autocorrelation coefficient calculation failed in",
                          test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }

     # -------------------------------------------------------      NumAcc1
     test.set <- "NumAcc1"
     results <- c(results,
                  paste("running", test.set))
     if (!(test.results$mean == test.results$NIST_mean)) {
          if (abs(test.results$mean - test.results$NIST_mean)
              > machine.error) {
               error.count <- error.count + 1
               error.message <- paste("Mean calculation failed in",
                                      test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }
     if (!(test.results$Standard_Deviation ==
           test.results$NIST_Standard_Deviation)) {
          if (abs(test.results$Standard_Deviation -
                  test.results$NIST_Standard_Deviation)
              > machine.error) {
               error.count <- error.count + 1
               error.message <-
                    paste("Standard deviation calculation failed in",
                          test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }
     if (!(test.results$Autocorrelation_Coefficient ==
           test.results$NIST_Autocorrelation_Coefficient)) {
          if (abs(test.results$Autocorrelation_Coefficient -
                  test.results$NIST_Autocorrelation_Coefficient)
              > machine.error) {
               error.count <- error.count + 1
               error.message <-
                    paste("Autocorrelation coefficient calculation failed in",
                          test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }

     # -------------------------------------------------------      Michelso
     test.set <- "Michelso"
     results <- c(results,
                  paste("running", test.set))
     test.results <- calculate.michelso()
     if (!(test.results$mean == test.results$NIST_mean)) {
          if (abs(test.results$mean - test.results$NIST_mean)
              > machine.error) {
               error.count <- error.count + 1
               error.message <- paste("Mean calculation failed in",
                                      test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }
     if (!(test.results$Standard_Deviation ==
           test.results$NIST_Standard_Deviation)) {
          if (abs(test.results$Standard_Deviation -
                  test.results$NIST_Standard_Deviation)
              > machine.error) {
               error.count <- error.count + 1
               error.message <-
                    paste("Standard deviation calculation failed in",
                          test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }
     if (!(test.results$Autocorrelation_Coefficient ==
           test.results$NIST_Autocorrelation_Coefficient)) {
          if (abs(test.results$Autocorrelation_Coefficient -
                  test.results$NIST_Autocorrelation_Coefficient)
              > machine.error) {
               error.count <- error.count + 1
               error.message <-
                    paste("Autocorrelation coefficient calculation failed in",
                          test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }

     # -------------------------------------------------------      Mavro
     test.set <- "Mavro"
     results <- c(results,
                  paste("running", test.set))
     test.results <- calculate.mavro()
     if (!(test.results$mean == test.results$NIST_mean)) {
          if (abs(test.results$mean - test.results$NIST_mean)
              > machine.error) {
               error.count <- error.count + 1
               error.message <- paste("Mean calculation failed in",
                                      test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }
     if (!(test.results$Standard_Deviation ==
           test.results$NIST_Standard_Deviation)) {
          if (abs(test.results$Standard_Deviation -
                  test.results$NIST_Standard_Deviation)
              > machine.error) {
               error.count <- error.count + 1
               error.message <-
                    paste("Standard deviation calculation failed in",
                          test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }
     if (!(test.results$Autocorrelation_Coefficient ==
           test.results$NIST_Autocorrelation_Coefficient)) {
          if (abs(test.results$Autocorrelation_Coefficient -
                  test.results$NIST_Autocorrelation_Coefficient)
              > machine.error) {
               error.count <- error.count + 1
               error.message <-
                    paste("Autocorrelation coefficient calculation failed in",
                          test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }

     # -------------------------------------------------------      Lottery
     test.set <- "Lottery"
     results <- c(results,
                  paste("running", test.set))
     test.results <- calculate.lottery()
     if (!(test.results$mean == test.results$NIST_mean)) {
          if (abs(test.results$mean - test.results$NIST_mean)
              > machine.error) {
               error.count <- error.count + 1
               error.message <- paste("Mean calculation failed in",
                                      test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }
     if (!(test.results$Standard_Deviation ==
           test.results$NIST_Standard_Deviation)) {
          if (abs(test.results$Standard_Deviation -
                  test.results$NIST_Standard_Deviation)
              > machine.error) {
               error.count <- error.count + 1
               error.message <-
                    paste("Standard deviation calculation failed in",
                          test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }
     if (!(test.results$Autocorrelation_Coefficient ==
           test.results$NIST_Autocorrelation_Coefficient)) {
          if (abs(test.results$Autocorrelation_Coefficient -
                  test.results$NIST_Autocorrelation_Coefficient)
              > machine.error) {
               error.count <- error.count + 1
               error.message <-
                    paste("Autocorrelation coefficient calculation failed in",
                          test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }

     # -------------------------------------------------------      Pi Digits
     test.set <- "Pi Digits"
     results <- c(results,
                  paste("running", test.set))
     test.results <- calculate.PiDigits()
     if (!(test.results$mean == test.results$NIST_mean)) {
          if (abs(test.results$mean - test.results$NIST_mean)
              > machine.error) {
               error.count <- error.count + 1
               error.message <- paste("Mean calculation failed in",
                                      test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }
     if (!(test.results$Standard_Deviation ==
           test.results$NIST_Standard_Deviation)) {
          if (abs(test.results$Standard_Deviation -
                  test.results$NIST_Standard_Deviation)
              > machine.error) {
               error.count <- error.count + 1
               error.message <-
                    paste("Standard deviation calculation failed in",
                          test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }
     if (!(test.results$Autocorrelation_Coefficient ==
           test.results$NIST_Autocorrelation_Coefficient)) {
          if (abs(test.results$Autocorrelation_Coefficient -
                  test.results$NIST_Autocorrelation_Coefficient)
              > machine.error) {
               error.count <- error.count + 1
               error.message <-
                    paste("Autocorrelation coefficient calculation failed in",
                          test.set)
               results <- c(results,
                            error.count,
                            error.message)
          }
     }

     results <- c(results,
                  paste("Errors encountered:",
                        error.count))

     return(results)
}
