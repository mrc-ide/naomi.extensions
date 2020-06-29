test_that("tmbstan fit returns results", {

  if ( !requireNamespace("INLA", quietly = TRUE) ) {
    testthat::skip("INLA package not installed")
  }

  INLA:::inla.dynload.workaround()

  CHAINS <- 2
  ITER <- 30

  ## suppressWarnings() because Stan will throw a bunch of convergence warnings as
  ## we are fitting with far too few iterations.
  stanfit1 <- suppressWarnings(
    fit_tmbstan(a_tmb_inputs, chains = CHAINS, iterations = ITER, rng_seed = 28)
  )
  stanfit1 <- sample_tmbstan(stanfit1)
  out1 <- naomi::output_package(stanfit1, a_naomi_data)

  stanfit2 <- suppressWarnings(
    fit_tmbstan(a_tmb_inputs, chains = CHAINS, iterations = ITER, rng_seed = 28)
  )
  stanfit2 <- sample_tmbstan(stanfit2)
  out2 <- naomi::output_package(stanfit2, a_naomi_data)

  stanfit3 <- suppressWarnings(
    fit_tmbstan(a_tmb_inputs, chains = CHAINS, iterations = ITER, rng_seed = 29)
  )
  stanfit3 <- sample_tmbstan(stanfit3)
  out3 <- naomi::output_package(stanfit3, a_naomi_data)

  expect_equal(ncol(stanfit1$sample$plhiv_t1), CHAINS * ITER / 2)
  expect_true(all(is.na(out1$indicators$mode)))
  expect_true(all(!is.na(out1$indicators[c("mean", "median", "se", "lower", "upper")])))
  expect_equal(out1$indicators, out2$indicators)
  expect_true(!all(out1$indicators$mean == out3$indicators$mean))

})

test_that("tmbstan with laplace returns results", {

  testthat::skip_on_covr()

  CHAINS <- 2
  ITER <- 4

  ## suppressWarnings() because Stan will throw a bunch of convergence warnings as
  ## we are fitting with far too few iterations.
  stanfit_laplace <- suppressWarnings(
    fit_tmbstan(a_tmb_inputs, chains = CHAINS, iterations = ITER, rng_seed = 29, laplace = TRUE)
  )
  stanfit_laplace <- sample_tmbstan(stanfit_laplace)
  out_laplace <- naomi::output_package(stanfit_laplace, a_naomi_data)

  expect_equal(ncol(stanfit_laplace$sample$plhiv_t1), CHAINS * ITER / 2)
  expect_true(all(is.na(out_laplace$indicators$mode)))
  expect_true(all(!is.na(out_laplace$indicators[c("mean", "median", "se", "lower", "upper")])))
})

test_that("INLA fit returns results", {
  testthat::skip("Test regressed and no longer working possible due to INLA update on 17/03/2020")


  INLA:::inla.dynload.workaround()

  ## NOTE: The error is related to new check in INLA for integer count data:
  ## * INLA.Data1: BINOMIAL likelihood is defined on integers, but y[4] = 1.16298220095
  ## This was introduced in INLA version 20.01.25:
  ## * https://bitbucket.org/hrue/r-inla/src/d74b5207d5473e40c0d7d6092f6a525d83403e04/rinla/inst/NEWS.Rd#lines-112
  ## Query raised on INLA discussion board, pending.

  inla_input <- prepare_inla_inputs(a_naomi_data)
  inla_fit <- fit_inla(inla_input, integration_strategy = "eb")
  inla_smp1 <- sample_inla(inla_fit, nsample = 20, rng_seed = 28)
  inla_smp2 <- sample_inla(inla_fit, nsample = 20, rng_seed = 28)
  inla_smp3 <- sample_inla(inla_fit, nsample = 20, rng_seed = 29)

  out1 <- naomi::output_package(inla_smp1, a_naomi_data)
  out2 <- naomi::output_package(inla_smp2, a_naomi_data)
  out3 <- naomi::output_package(inla_smp3, a_naomi_data)

  expect_equal(out1$indicators, out2$indicators)
  expect_true(!all(out1$indicators$mean == out3$indicators$mean, na.rm = TRUE))

})
