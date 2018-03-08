# ---------------------------------------------------------------Lew
context("lew Univarent Statistics")

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


# ---------------------------------------------------------------NumAcc4
context("NumAcc4 Univarent Statistics")

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


# ---------------------------------------------------------------NumAcc3
context("NumAcc3 Univarent Statistics")

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


# ---------------------------------------------------------------NumAcc2
context("NumAcc2 Univarent Statistics")

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


# ---------------------------------------------------------------NumAcc1
context("NumAcc1 Univarent Statistics")

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


# ---------------------------------------------------------------michelso
context("michelso Univarent Statistics")

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


# ---------------------------------------------------------------mavro
context("mavro Univarent Statistics")

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


# ---------------------------------------------------------------lottery
context("lottery Univarent Statistics")

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


# --------------------------------------------------------------Pi Digits
context("PiDigits Univarent Statistics")

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
