test_that("parameter function works for full draws matrix", {
  expect_equal(
    parameter(test.mcmc.obj, "lambda"),
    test.draws[, c(1, 2)],
    ignore_attr = TRUE
  )
})

test_that("parameter function works for partial draws matrix", {
  expect_equal(
    parameter(test.mcmc.obj, "lambda", indices_to_extract = 1),
    coda::as.mcmc(test.draws[, 1], start = 1, end = 10, thin = 1),
    ignore_attr = TRUE
  )
})

test_that("parameter function works for alpha.sq", {
  expect_equal(
    parameter(test.mcmc.obj, parameter_name = "alpha.sq"),
    coda::as.mcmc(test.draws[, 2], start = 1, end = 10, thin = 1),
    ignore_attr = TRUE
  )
})

test_that("check_model works when object is valid", {
  expect_no_error(check_model(test.mcmc.obj, parameter_name = "lambda"))
})


test_that("check_model fails when object is invalid", {
  expect_error(
    check_model(matrix(0, 1, 1), parameter_name = "lambda"),
    "model_output must be an object of class mcmc for this function to work correctly."
  )
})


test_that("check_model fails when parameter name isn't found in the draws matrix", {
  expect_error(
    check_model(test.bad.obj, parameter_name = "lambda"),
    "This model object does not contain estimates of the lambda parameter. Please run a model using the `speedyBBTm` function."
  )
})
