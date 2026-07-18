test_that("speedyBBTm produces results within tolerance", {
  # Construct covariance matrix
  data("forcedMarriage")
  expA <- expm::expm(forcedMarriage$adjacencyMatrix)
  sigma <- diag(diag(expA)^-0.5) %*% expA %*% diag(diag(expA)^-0.5)

  # Fit model
  forcedMarriageModel <- speedyBBTm(
    outcome = rep(1, length(forcedMarriage$comparisons$win)),
    player1 = forcedMarriage$comparisons$win,
    player2 = forcedMarriage$comparisons$lost,
    player.prior.var = sigma,
    n.iter = 10000
  )

  forcedMarriageModelMeans <- colMeans(forcedMarriageModel$lambda[-c(1:100), ])

  # Read in means
  testMeansPath <- test_path("forcedMarriageModelMeans.csv")
  testMeans <- read.csv(testMeansPath)

  expect_equal(
    sum(abs(testMeans - forcedMarriageModelMeans)) / 76,
    0,
    tolerance = 1e-1
  )
})


test_that("BBTm produces results within tolerance", {
  # Construct covariance matrix
  # Fit model
  wimbledonModel <- BBTm(
    outcome = wimbledon$matches$outcome,
    player1 = wimbledon$matches$winner,
    player2 = wimbledon$matches$loser,
    advantage = wimbledon$matches$secondWeek,
    formula = ~ rank + points,
    data = wimbledon$players,
    n.iter = 4000
  )

  wimbledonModelMeans <- colMeans(wimbledonModel$lambda[-c(1:50), ])

  # Read in means
  testMeansPath <- test_path("wimbledonModelMeans.csv")
  testMeans <- read.csv(testMeansPath)

  # Compare within tolerance
  expect_equal(
    sum(abs(testMeans - wimbledonModelMeans)) / 128,
    0,
    tolerance = 1e-1
  )
})

test_that("BBTm.ties produces results within tolerance", {
  # Construct covariance matrix
  # Fit model
  data("darEsSalaam")

  sigma <- expm::expm(darEsSalaam$adjacencyMatrix)
  sigma <- diag(diag(sigma)^-0.5) %*% sigma %*% diag(diag(sigma)^-0.5)
  n.objects <- 452
  darTiedModel <- BBTm.ties(
    n.objects = n.objects,
    outcome = darEsSalaam$comparisons$outcome,
    player1 = darEsSalaam$comparisons$subward1,
    player2 = darEsSalaam$comparisons$subward2,
    player.prior.var = sigma,
    hyperparameter = TRUE,
    rw.sd = 0.005
  )
  # Get posterior means
  centered_lambda <- darTiedModel$lambda - colMeans(darTiedModel$lambda)
  lambda.mean <- rowMeans(centered_lambda)

  # Read in means
  testMeansPath <- test_path("darTiedModelMeans.csv")
  testMeans <- read.csv(testMeansPath)

  # Compare within tolerance
  expect_equal(
    sum(abs(testMeans - lambda.mean)) / n.objects,
    0,
    tolerance = 1e-1
  )
})
