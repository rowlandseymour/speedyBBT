test_that("speedyBBTm produces results within tolerance", {
  # Construct covariance matrix
  set.seed(905)
  expA <- expm::expm(forcedMarriage$adjacencyMatrix)
  prior.var <- diag(diag(expA)^-0.5) %*% expA %*% diag(diag(expA)^-0.5)

  # Fit model
  forcedMarriageModel <- speedyBBTm(
    outcome = rep(1, length(forcedMarriage$comparisons$win)),
    item1 = forcedMarriage$comparisons$win,
    item2 = forcedMarriage$comparisons$lost,
    item.prior.var = prior.var,
    n.iter = 2000
  )

  forcedMarriageModelMeans <- colMeans(forcedMarriageModel[, grep(
    "lambda",
    varnames(forcedMarriageModel)
  )])

  # Read in means
  testMeansPath <- test_path("forcedMarriageModelMeans.csv")
  testMeans <- read.csv(testMeansPath)

  expect_equal(
    sum(abs(testMeans - forcedMarriageModelMeans)) /
      nrow(forcedMarriage$adjacencyMatrix),
    0,
    tolerance = 1e-1
  )

  forcedMarriageModelMeanAlphaSq <- mean(forcedMarriageModel[, grep(
    "alpha.sq",
    varnames(forcedMarriageModel)
  )])

  expect_equal(
    abs(13.48986 - forcedMarriageModelMeanAlphaSq),
    0,
    tolerance = 1e-1
  )
})

test_that("speedyBBTm produces an error if n.iter is less than the burn.in period", {
  set.seed(905)
  expA <- expm::expm(forcedMarriage$adjacencyMatrix)
  prior.var <- diag(diag(expA)^-0.5) %*% expA %*% diag(diag(expA)^-0.5)

  # Fit model
  expect_error(
    forcedMarriageModel <- speedyBBTm(
      outcome = rep(1, length(forcedMarriage$comparisons$win)),
      item1 = forcedMarriage$comparisons$win,
      item2 = forcedMarriage$comparisons$lost,
      item.prior.var = prior.var,
      n.iter = 2,
      burn.in = 3
    )
  )
})

test_that("speedyBBTm produces a warning but still runs when a deprecated argument is used", {
  # Construct covariance matrix
  set.seed(905)
  expA <- expm::expm(forcedMarriage$adjacencyMatrix)
  prior.var <- diag(diag(expA)^-0.5) %*% expA %*% diag(diag(expA)^-0.5)

  # Fit model
  expect_warning(
    forcedMarriageModel <- speedyBBTm(
      outcome = rep(1, length(forcedMarriage$comparisons$win)),
      player1 = forcedMarriage$comparisons$win,
      player2 = forcedMarriage$comparisons$lost,
      player.prior.var = prior.var,
      n.iter = 2,
      burn.in = 0
    )
  )

  expect_s3_class(forcedMarriageModel, "mcmc")
})

test_that("BBTm produces an error when n.iter < n.burn.in", {
  # Construct covariance matrix
  # Fit model
  expect_error(
    wimbledonModel <- BBTm(
      outcome = wimbledon$matches$outcome,
      item1 = wimbledon$matches$winner,
      item2 = wimbledon$matches$loser,
      advantage = wimbledon$matches$secondWeek,
      formula = ~ rank + points,
      data = wimbledon$players,
      n.iter = 2,
      burn.in = 3
    )
  )
})


test_that("BBTm produces results within tolerance when advantage = TRUE and hyperparameter = TRUE", {
  # Construct covariance matrix
  # Fit model
  wimbledonModel <- BBTm(
    outcome = wimbledon$matches$outcome,
    item1 = wimbledon$matches$winner,
    item2 = wimbledon$matches$loser,
    advantage = wimbledon$matches$secondWeek,
    formula = ~ rank + points,
    data = wimbledon$players,
    n.iter = 4000
  )

  wimbledonModelMeans <- colMeans(parameter(wimbledonModel, "lambda")[
    -c(1:50),
  ])

  # Read in means
  testMeansPath <- test_path("wimbledonModelMeans.csv")
  testMeans <- read.csv(testMeansPath)

  # Compare within tolerance
  expect_equal(
    sum(abs(testMeans - wimbledonModelMeans)) / nrow(wimbledon$players),
    0,
    tolerance = 1e-1
  )
})

test_that("BBTm produces a warning but still runs when a deprecated argument is used", {
  set.seed(905)
  expA <- expm::expm(forcedMarriage$adjacencyMatrix)
  prior.var <- diag(diag(expA)^-0.5) %*% expA %*% diag(diag(expA)^-0.5)

  # Fit model
  expect_warning(
    # Fit model
    forcedMarriageModel <- BBTm(
      outcome = rep(1, length(forcedMarriage$comparisons$win)),
      player1 = forcedMarriage$comparisons$win,
      player2 = forcedMarriage$comparisons$lost,
      player.prior.var = prior.var,
      n.iter = 1000
    )
  )
})

test_that("BBTm produces results within tolerance when hyperparameter = FALSE and advantage = TRUE", {
  # Construct covariance matrix
  # Fit model
  mod_run <- function() {
    set.seed(423)
    wimbledonModel <- BBTm(
      outcome = wimbledon$matches$outcome,
      item1 = wimbledon$matches$winner,
      item2 = wimbledon$matches$loser,
      advantage = wimbledon$matches$secondWeek,
      formula = ~ rank + points,
      data = wimbledon$players,
      n.iter = 2000,
      hyperparameter = FALSE
    )

    wimbledonModelMeans <- colMeans(parameter(wimbledonModel, "lambda")[
      -c(1:50),
    ])
    return(wimbledonModelMeans)
  }
  expect_snapshot_value(
    mod_run(),
    style = "serialize",
    tolerance = 1e-4
  )
})

test_that("BBTm produces results within tolerance when hyperparameter = TRUE and advantage = FALSE", {
  # Construct covariance matrix
  # Fit model
  mod_run <- function() {
    set.seed(423)
    wimbledonModel <- BBTm(
      outcome = wimbledon$matches$outcome,
      item1 = wimbledon$matches$winner,
      item2 = wimbledon$matches$loser,
      formula = ~ rank + points,
      data = wimbledon$players,
      n.iter = 2000,
      hyperparameter = TRUE
    )

    wimbledonModelMeans <- colMeans(parameter(wimbledonModel, "lambda")[
      -c(1:50),
    ])
    return(wimbledonModelMeans)
  }
  expect_snapshot_value(
    mod_run(),
    style = "serialize",
    tolerance = 1e-4
  )
})

test_that("BBTm produces results within tolerance when hyperparameter = FALSE and advantage = FALSE", {
  # Construct covariance matrix
  # Fit model
  mod_run <- function() {
    set.seed(423)
    wimbledonModel <- BBTm(
      outcome = wimbledon$matches$outcome,
      item1 = wimbledon$matches$winner,
      item2 = wimbledon$matches$loser,
      formula = ~ rank + points,
      data = wimbledon$players,
      n.iter = 4000,
      hyperparameter = FALSE
    )

    wimbledonModelMeans <- colMeans(parameter(wimbledonModel, "lambda")[
      -c(1:50),
    ])
    return(wimbledonModelMeans)
  }

  expect_snapshot_value(
    mod_run(),
    style = "serialize",
    tolerance = 1e-4
  )
})

test_that("BBTM.with.formula produces a warning but still runs when you use deprecated arguments", {
  # Construct covariance matrix
  # Fit model
  expect_warning(
    wimbledonModel <- BBTm.with.formula(
      outcome = wimbledon$matches$outcome,
      player1 = wimbledon$matches$winner,
      player2 = wimbledon$matches$loser,
      formula = ~ rank + points,
      data = wimbledon$players,
      n.iter = 4000,
      hyperparameter = FALSE
    )
  )
})


test_that("BBTm.no.formula produces results within tolerance", {
  # Construct covariance matrix
  # Fit model
  set.seed(332)
  # Construct covariance matrix
  expA <- expm::expm(forcedMarriage$adjacencyMatrix)
  prior.var <- diag(diag(expA)^-0.5) %*% expA %*% diag(diag(expA)^-0.5)

  # Fit model
  forcedMarriageModel <- BBTm(
    outcome = rep(1, length(forcedMarriage$comparisons$win)),
    item1 = forcedMarriage$comparisons$win,
    item2 = forcedMarriage$comparisons$lost,
    item.prior.var = prior.var,
    n.iter = 1000
  )

  lambda_means <- colMeans(forcedMarriageModel[
    ,
    grep(
      "lambda",
      varnames(forcedMarriageModel)
    )
  ])

  # Read in means
  testMeansPath <- test_path("forcedMarriageModelMeansNoFormula.csv")
  testMeans <- read.csv(testMeansPath)

  expect_equal(
    sum(abs(testMeans - lambda_means)) /
      nrow(forcedMarriage$adjacencyMatrix),
    0,
    tolerance = 1e-1
  )
})

test_that("BBTm.no.formula without advantage and hyperparameter=FALSE produces results within tolerance", {
  mod_run <- function() {
    set.seed(42)
    expA <- expm::expm(forcedMarriage$adjacencyMatrix)
    prior.var <- diag(diag(expA)^-0.5) %*% expA %*% diag(diag(expA)^-0.5)

    model <- BBTm.no.formula(
      outcome = rep(1, length(forcedMarriage$comparisons$win)),
      item1 = forcedMarriage$comparisons$win,
      item2 = forcedMarriage$comparisons$lost,
      item.prior.var = prior.var,
      lambda.initial = numeric(nrow(forcedMarriage$adjacencyMatrix)),
      n.iter = 1000,
      burn.in = 100,
      hyperparameter = FALSE,
      verbose = FALSE
    )

    model_means <- colMeans(model)
    return(model_means)
  }
  expect_snapshot_value(
    mod_run(),
    style = "serialize",
    tolerance = 1e-4
  )
})

test_that("BBTm.no.formula with advantage and hyperparameter=FALSE produces results within tolerance", {
  mod_run <- function() {
    set.seed(42)
    expA <- expm::expm(forcedMarriage$adjacencyMatrix)
    prior.var <- diag(diag(expA)^-0.5) %*% expA %*% diag(diag(expA)^-0.5)
    advantage <- rep(1, length(forcedMarriage$comparisons$win))

    model <- BBTm.no.formula(
      outcome = rep(1, length(forcedMarriage$comparisons$win)),
      item1 = forcedMarriage$comparisons$win,
      item2 = forcedMarriage$comparisons$lost,
      item.prior.var = prior.var,
      lambda.initial = numeric(nrow(forcedMarriage$adjacencyMatrix)),
      advantage = advantage,
      n.iter = 10,
      burn.in = 1,
      hyperparameter = FALSE,
      verbose = FALSE
    )

    model_means <- colMeans(model)
    return(model_means)
  }
  expect_snapshot_value(
    mod_run(),
    style = "serialize",
    tolerance = 1e-4
  )
})

test_that("BBTm.no.formula with advantage and hyperparameter=TRUE produces results within tolerance", {
  mod_run <- function() {
    set.seed(42)
    expA <- expm::expm(forcedMarriage$adjacencyMatrix)
    prior.var <- diag(diag(expA)^-0.5) %*% expA %*% diag(diag(expA)^-0.5)
    advantage <- rep(1, length(forcedMarriage$comparisons$win))

    model <- BBTm.no.formula(
      outcome = rep(1, length(forcedMarriage$comparisons$win)),
      item1 = forcedMarriage$comparisons$win,
      item2 = forcedMarriage$comparisons$lost,
      item.prior.var = prior.var,
      lambda.initial = numeric(nrow(forcedMarriage$adjacencyMatrix)),
      advantage = advantage,
      n.iter = 10,
      burn.in = 1,
      hyperparameter = TRUE,
      verbose = FALSE
    )

    model_means <- colMeans(model)
    return(model_means)
  }
  expect_snapshot_value(
    mod_run(),
    style = "serialize",
    tolerance = 1e-4
  )
})

test_that("BBTm.ties produces expected output from two iterations", {
  # Construct covariance matrix
  # Fit model
  mod_run <- function() {
    set.seed(123)
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
      burn.in = 0,
      n.iter = 2
    )
    # Get posterior means
    centered_lambda <- parameter(darTiedModel, "lambda") -
      colMeans(parameter(darTiedModel, "lambda"))
    lambda.mean <- rowMeans(centered_lambda)
    theta.mean <- mean(parameter(darTiedModel, "theta"))
    alpha.sq.mean <- mean(parameter(darTiedModel, "alpha.sq"))

    return(c(lambda.mean, theta.mean, alpha.sq.mean))
  }
  expect_snapshot_value(
    mod_run(),
    style = "serialize",
    tolerance = 1e-4
  )
})

test_that("BBTm.ties produces an error when n.iter < burn.in", {
  prior.var <- expm::expm(darEsSalaam$adjacencyMatrix)
  prior.var <- diag(diag(prior.var)^-0.5) %*%
    prior.var %*%
    diag(diag(prior.var)^-0.5)
  n.objects <- nrow(darEsSalaam$adjacencyMatrix)
  expect_error(
    darTiedModel <- BBTm.ties(
      n.objects = n.objects,
      outcome = darEsSalaam$comparisons$outcome,
      item1 = darEsSalaam$comparisons$subward1,
      item2 = darEsSalaam$comparisons$subward2,
      item.prior.var = prior.var,
      hyperparameter = TRUE,
      rw.sd = 0.005,
      burn.in = 3,
      n.iter = 2
    )
  )
})

test_that("BBTm.ties produces a warning but still runs when a deprecated argument is used", {
  set.seed(123)
  prior.var <- expm::expm(darEsSalaam$adjacencyMatrix)
  prior.var <- diag(diag(prior.var)^-0.5) %*%
    prior.var %*%
    diag(diag(prior.var)^-0.5)
  n.objects <- nrow(darEsSalaam$adjacencyMatrix)

  # Fit model
  expect_warning(
    darTiedModel <- BBTm.ties(
      n.objects = n.objects,
      outcome = darEsSalaam$comparisons$outcome,
      player1 = darEsSalaam$comparisons$subward1,
      player2 = darEsSalaam$comparisons$subward2,
      player.prior.var = prior.var,
      hyperparameter = TRUE,
      rw.sd = 0.005,
      burn.in = 0,
      n.iter = 2
    )
  )
})

test_that("BBTm.ties produces expected output from two iterations when hyperparameter = FALSE", {
  # Construct covariance matrix
  # Fit model

  mod_run <- function() {
    set.seed(123)
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
      hyperparameter = FALSE,
      rw.sd = 0.005,
      burn.in = 0,
      n.iter = 2
    )
    # Get posterior means
    centered_lambda <- parameter(darTiedModel, "lambda") -
      colMeans(parameter(darTiedModel, "lambda"))
    lambda.mean <- rowMeans(centered_lambda)

    theta.mean <- mean(parameter(darTiedModel, "theta"))
    return(c(lambda.mean, theta.mean))
  }

  # Read in means
  expect_snapshot_value(
    mod_run(),
    style = "serialize",
    tolerance = 1e-4
  )
})
