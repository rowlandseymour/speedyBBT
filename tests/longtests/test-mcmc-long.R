test_that("BBTm.ties produces results within tolerance", {
  # Construct covariance matrix
  # Fit model

  prior.var <- expm::expm(darEsSalaam$adjacencyMatrix)
  prior.var <- diag(diag(prior.var)^-0.5) %*%
    prior.var %*%
    diag(diag(prior.var)^-0.5)
  n.objects <- nrow(darEsSalaam$adjacencyMatrix)
  darTiedModel <- BBTm.ties(
    n.objects = n.objects,
    outcome = darEsSalaam$comparisons$outcome,
    item1 = darEsSalaam$comparisons$subward1,
    item2 = darEsSalaam$comparisons$subward2,
    item.prior.var = prior.var,
    hyperparameter = TRUE,
    rw.sd = 0.005,
    n.iter = 400
  )

  # Get posterior means
  centered_lambda <- parameter(darTiedModel, "lambda") -
    colMeans(parameter(darTiedModel, "lambda"))
  lambda.mean <- colMeans(centered_lambda)

  # Read in means
  testMeans <- read.csv("darTiedModelMeansFullRun.csv")

  # Compare within tolerance
  expect_equal(
    sum(abs(testMeans - lambda.mean)) / n.objects,
    0,
    tolerance = 1e-1
  )
})
