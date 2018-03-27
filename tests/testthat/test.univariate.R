# <<<<<<<<<<<<<<<<<<<<Lew
context("lew Univariate Statistics")

test_that("output", {
     expect_output(str(calculate.lew()), "1 obs")
     expect_output(str(calculate.lew()), "7 variables")
     expect_silent(calculate.lew())
     expect_output(str(calculate.lew(), "data.frame"))
})

test_that("values", {
     expect_equal(calculate.lew()$NIST, "Lew")
     expect_equal(calculate.lew()$mean, calculate.lew()$NIST_mean)
     expect_equal(
          calculate.lew()$Standard_Deviation,
          calculate.lew()$NIST_Standard_Deviation
     )
     expect_equal(
          calculate.lew()$Autocorrelation_Coefficient,
          calculate.lew()$NIST_Autocorrelation_Coefficient
     )
})

test_that("NIST Values", {
     expect_identical(calculate.lew()$NIST_mean,
                      -177.435000000000)
     expect_identical(calculate.lew()$NIST_Standard_Deviation,
                      277.332168044316)
     expect_identical(calculate.lew()$NIST_Autocorrelation_Coefficient,
                      -0.307304800605679)
})


# <<<<<<<<<<<<<<<<<<<<NumAcc4
context("NumAcc4 Univariate Statistics")

test_that("output", {
     expect_output(str(calculate.NumAcc4()), "1 obs")
     expect_output(str(calculate.NumAcc4()), "7 variables")
     expect_silent(calculate.NumAcc4())
     expect_output(str(calculate.NumAcc4(), "data.frame"))
})

test_that("values", {
     expect_equal(calculate.NumAcc4()$NIST, "NumAcc4")
     expect_equal(calculate.NumAcc4()$mean,
                  calculate.NumAcc4()$NIST_mean)
     expect_equal(
          calculate.NumAcc4()$Standard_Deviation,
          calculate.NumAcc4()$NIST_Standard_Deviation
     )
     expect_equal(
          calculate.NumAcc4()$Autocorrelation_Coefficient,
          calculate.NumAcc4()$NIST_Autocorrelation_Coefficient
     )
})

test_that("NIST Values", {
     expect_identical(calculate.NumAcc4()$NIST_mean,
                      10000000.2)
     expect_identical(calculate.NumAcc4()$NIST_Standard_Deviation,
                      0.1)
     expect_identical(calculate.NumAcc4()$NIST_Autocorrelation_Coefficient,
                      -0.999)
})


# <<<<<<<<<<<<<<<<<<<<NumAcc3
context("NumAcc3 Univariate Statistics")

test_that("output", {
     expect_output(str(calculate.NumAcc3()), "1 obs")
     expect_output(str(calculate.NumAcc3()), "7 variables")
     expect_silent(calculate.NumAcc3())
     expect_output(str(calculate.NumAcc3(), "data.frame"))
})

test_that("values", {
     expect_equal(calculate.NumAcc3()$NIST, "NumAcc3")
     expect_equal(calculate.NumAcc3()$mean,
                  calculate.NumAcc3()$NIST_mean)
     expect_equal(
          calculate.NumAcc3()$Standard_Deviation,
          calculate.NumAcc3()$NIST_Standard_Deviation
     )
     expect_equal(
          calculate.NumAcc3()$Autocorrelation_Coefficient,
          calculate.NumAcc3()$NIST_Autocorrelation_Coefficient
     )
})

test_that("NIST Values", {
     expect_identical(calculate.NumAcc3()$NIST_mean,
                      1000000.2)
     expect_identical(calculate.NumAcc3()$NIST_Standard_Deviation,
                      0.1)
     expect_identical(calculate.NumAcc3()$NIST_Autocorrelation_Coefficient,
                      -0.999)
})


# <<<<<<<<<<<<<<<<<<<<NumAcc2
context("NumAcc2 Univariate Statistics")

test_that("output", {
     expect_output(str(calculate.NumAcc2()), "1 obs")
     expect_output(str(calculate.NumAcc2()), "7 variables")
     expect_silent(calculate.NumAcc2())
     expect_output(str(calculate.NumAcc2(), "data.frame"))
})

test_that("values", {
     expect_equal(calculate.NumAcc2()$NIST, "NumAcc2")
     expect_equal(calculate.NumAcc2()$mean,
                  calculate.NumAcc2()$NIST_mean)
     expect_equal(
          calculate.NumAcc2()$Standard_Deviation,
          calculate.NumAcc2()$NIST_Standard_Deviation
     )
     expect_equal(
          calculate.NumAcc2()$Autocorrelation_Coefficient,
          calculate.NumAcc2()$NIST_Autocorrelation_Coefficient
     )
})

test_that("NIST Values", {
     expect_identical(calculate.NumAcc2()$NIST_mean,
                      1.2)
     expect_identical(calculate.NumAcc2()$NIST_Standard_Deviation,
                      0.1)
     expect_identical(calculate.NumAcc2()$NIST_Autocorrelation_Coefficient,
                      -0.999)
})


# <<<<<<<<<<<<<<<<<<<<NumAcc1
context("NumAcc1 Univariate Statistics")

test_that("output", {
     expect_output(str(calculate.NumAcc1()), "1 obs")
     expect_output(str(calculate.NumAcc1()), "7 variables")
     expect_silent(calculate.NumAcc1())
     expect_output(str(calculate.NumAcc1(), "data.frame"))
})

test_that("values", {
     expect_equal(calculate.NumAcc1()$NIST, "NumAcc1")
     expect_equal(calculate.NumAcc1()$mean,
                  calculate.NumAcc1()$NIST_mean)
     expect_equal(
          calculate.NumAcc1()$Standard_Deviation,
          calculate.NumAcc1()$NIST_Standard_Deviation
     )
     expect_equal(
          calculate.NumAcc1()$Autocorrelation_Coefficient,
          calculate.NumAcc1()$NIST_Autocorrelation_Coefficient
     )
})

test_that("NIST Values", {
     expect_identical(calculate.NumAcc1()$NIST_mean,
                      10000002)
     expect_identical(calculate.NumAcc1()$NIST_Standard_Deviation,
                      1)
     expect_identical(calculate.NumAcc1()$NIST_Autocorrelation_Coefficient,
                      -0.5)
})


# <<<<<<<<<<<<<<<<<<<<michelso
context("michelso Univariate Statistics")

test_that("output", {
     expect_output(str(calculate.michelso()), "1 obs")
     expect_output(str(calculate.michelso()), "7 variables")
     expect_silent(calculate.michelso())
     expect_output(str(calculate.michelso(), "data.frame"))
})

test_that("values", {
     expect_equal(calculate.michelso()$NIST, "Michelso")
     expect_equal(calculate.michelso()$mean,
                  calculate.michelso()$NIST_mean)
     expect_equal(
          calculate.michelso()$Standard_Deviation,
          calculate.michelso()$NIST_Standard_Deviation
     )
     expect_equal(
          calculate.michelso()$Autocorrelation_Coefficient,
          calculate.michelso()$NIST_Autocorrelation_Coefficient
     )
})

test_that("NIST Values", {
     expect_identical(calculate.michelso()$NIST_mean,
                      299.852400000000)
     expect_identical(calculate.michelso()$NIST_Standard_Deviation,
                      0.0790105478190518)
     expect_identical(calculate.michelso()$NIST_Autocorrelation_Coefficient,
                      0.535199668621283)
})


# <<<<<<<<<<<<<<<<<<<<mavro
context("mavro Univariate Statistics")

test_that("output", {
     expect_output(str(calculate.mavro()), "1 obs")
     expect_output(str(calculate.mavro()), "7 variables")
     expect_silent(calculate.mavro())
     expect_output(str(calculate.mavro(), "data.frame"))
})

test_that("values", {
     expect_equal(calculate.mavro()$NIST, "Mavro")
     expect_equal(calculate.mavro()$mean,
                  calculate.mavro()$NIST_mean)
     expect_equal(
          calculate.mavro()$Standard_Deviation,
          calculate.mavro()$NIST_Standard_Deviation
     )
     expect_equal(
          calculate.mavro()$Autocorrelation_Coefficient,
          calculate.mavro()$NIST_Autocorrelation_Coefficient
     )
})

test_that("NIST Values", {
     expect_identical(calculate.mavro()$NIST_mean,
                      2.00185600000000)
     expect_identical(calculate.mavro()$NIST_Standard_Deviation,
                      0.000429123454003053)
     expect_identical(calculate.mavro()$NIST_Autocorrelation_Coefficient,
                      0.937989183438248)
})


# <<<<<<<<<<<<<<<<<<<<lottery
context("lottery Univariate Statistics")

test_that("output", {
     expect_output(str(calculate.lottery()), "1 obs")
     expect_output(str(calculate.lottery()), "7 variables")
     expect_silent(calculate.lottery())
     expect_output(str(calculate.lottery(), "data.frame"))
})

test_that("values", {
     expect_equal(calculate.lottery()$NIST, "Lottery")
     expect_equal(calculate.lottery()$mean,
                  calculate.lottery()$NIST_mean)
     expect_equal(
          calculate.lottery()$Standard_Deviation,
          calculate.lottery()$NIST_Standard_Deviation
     )
     expect_equal(
          calculate.lottery()$Autocorrelation_Coefficient,
          calculate.lottery()$NIST_Autocorrelation_Coefficient
     )
})

test_that("NIST Values", {
     expect_identical(calculate.lottery()$NIST_mean,
                      518.958715596330)
     expect_identical(calculate.lottery()$NIST_Standard_Deviation,
                      291.699727470969)
     expect_identical(calculate.lottery()$NIST_Autocorrelation_Coefficient,
                      -0.120948622967393)
})


# <<<<<<<<<<<<<<<<<<<<PiDigits
context("PiDigits Univariate Statistics")

test_that("output", {
     expect_output(str(calculate.PiDigits()), "1 obs")
     expect_output(str(calculate.PiDigits()), "7 variables")
     expect_silent(calculate.PiDigits())
     expect_output(str(calculate.PiDigits(), "data.frame"))
})

test_that("values", {
     expect_equal(calculate.PiDigits()$NIST, "PiDigits")
     expect_equal(calculate.PiDigits()$mean,
                  calculate.PiDigits()$NIST_mean)
     expect_equal(
          calculate.PiDigits()$Standard_Deviation,
          calculate.PiDigits()$NIST_Standard_Deviation
     )
     expect_equal(
          calculate.PiDigits()$Autocorrelation_Coefficient,
          calculate.PiDigits()$NIST_Autocorrelation_Coefficient
     )
})


test_that("NIST Values", {
     expect_identical(calculate.PiDigits()$NIST_mean,
                      4.53480000000000)
     expect_identical(calculate.PiDigits()$NIST_Standard_Deviation,
                      2.86733906028871)
     expect_identical(calculate.PiDigits()$NIST_Autocorrelation_Coefficient,
                      -0.00355099287237972)
})

# <<<<<<<<<<<<<<<<<<<< Check univ
context("Check univ")

test_that("output", {
   expect_silent(check.univariate())
   expect_silent(check.univariate(print = FALSE))
   expect_silent(check.univariate(print = F))
   expect_silent(check.univariate(print = TRUE))
   expect_silent(check.univariate(print = T))
   expect_silent(check.univariate(FALSE))
   expect_silent(check.univariate(F))
   expect_silent(check.univariate(T))
   expect_silent(check.univariate(TRUE))
   expect_output(str(check.univariate()), "chr")
})

test_that("Error Trapping", {
   expect_warning(check.univariate(print = 1))
   expect_warning(check.univariate(print = "A"))

})
