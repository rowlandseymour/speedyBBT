test_that("BBTm.ties produces results within tolerance", {
  # Construct covariance matrix
  # Fit model

  sigma <- expm::expm(darEsSalaam$adjacencyMatrix)
  sigma <- diag(diag(sigma)^-0.5) %*% sigma %*% diag(diag(sigma)^-0.5)
  n.objects <- nrow(darEsSalaam$adjacencyMatrix)
  darTiedModel <- BBTm.ties(
    n.objects = n.objects,
    outcome = darEsSalaam$comparisons$outcome,
    player1 = darEsSalaam$comparisons$subward1,
    player2 = darEsSalaam$comparisons$subward2,
    player.prior.var = sigma,
    hyperparameter = TRUE,
    rw.sd = 0.005,
    n.iter = 400
  )

  # Get posterior means
  centered_lambda <- parameter(darTiedModel, "lambda") -
    colMeans(parameter(darTiedModel, "lambda"))
  lambda.mean <- rowMeans(centered_lambda)

  # Read in means
  testMeans <- read.csv("darTiedModelMeansFullRun.csv")

  # Compare within tolerance
  expect_equal(
    sum(abs(testMeans - lambda.mean)) / n.objects,
    0,
    tolerance = 1e-1
  )
})
