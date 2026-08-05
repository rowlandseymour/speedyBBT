#' Standard Bayesian Bradley-Terry model
#'
#' This function uses MCMC to sample from the posterior distribution of the
#' standard Bradley--Terry model. Standard model means that there are no tied
#' comparisons and no item or comparison specific variables. This provides a
#' fast implementation of the standard model. A multivariate normal prior
#' distribution on the item quality parameters can be specified.
#'
#'
#'
#' @param outcome Vector of outcomes. 1 if item 2 is the winner,
#'  0 if item 1 is the winner.
#' @param item1 Vector of first items.
#' @param item2 Vector of second items.
#' @param win.matrix (optional) A win-loss matrix where the i,j th element is the number of
#' times object i beat object j.
#' @param item.prior.var (optional) A matrix specifying the prior covariance of
#'  the item correlation parameters.
#' @param lambda.initial (optional) A vector containing the values of the
#'  item correlation parameters for the first MCMC iteration.
#' @param n.iter The number of MCMC samples to be drawn.
#' @param hyperparameter A boolean indicating if inference should be performed
#'  for the prior variance hyperparameter. If TRUE the prior variance
#'  (main diagonal of the covariance matrix) must be set to 1.
#' @param psi (Optional) The shape parameter for the inverse-gamma prior distribution on the
#' hyperparameter. Default is 0.01.
#' @param chi (Optional) The rate parameter for the inverse-gamma prior distribution on the
#'  hyperparameter. Default is 0.01.
#' @param burn.in (optional) The number of iterations to discard as burn-in. Default is 100.
#'
#' @details If `item.prior.var` is omitted, independent and identical
#' N(0, 1^2) prior distributions are placed on each object quality parameter.
#'
#' If `lambda.initial` is ommitted, it is set to a vector of zeroes.
#'
#'
#' @return  An ["mcmc"][coda::mcmc] object containing samples from the posterior distribution.
#'
#' @importFrom coda mcmc varnames
#'
#' @examples
#' \donttest{
#' ########################################
#' ## Forced Marriage in Nottinghamshire ##
#' ########################################
#' # Construct covariance matrix based on spatial information
#' sigma <- expm::expm(forcedMarriage$adjacencyMatrix)
#' sigma <- diag(diag(sigma)^-0.5) %*% sigma %*% diag(diag(sigma)^-0.5)
#'
#' # Fit model
#' # Using `n.iter = 3` here to reduce runtime, you will need more
#' # iterations for inference.
#' forcedMarriageModel <- speedyBBTm(
#'   outcome = rep(1, length(forcedMarriage$comparisons$win)),
#'   item1 = forcedMarriage$comparisons$win,
#'   item2 = forcedMarriage$comparisons$lost,
#'   item.prior.var = sigma, n.iter = 3, burn.in = 0
#' )
#'
#' # Plot results
#' oldpar <- par(mfrow = c(2, 2))
#'
#' plot(forcedMarriageModel[, paste0("lambda[", c(10, 20, 30, 40), "]")], xlab = "Iteration", ylab = expression(lambda[i]))
#' par(oldpar)
#' }
#' @export
#'
speedyBBTm <- function(
  outcome = NULL,
  item1 = NULL,
  item2 = NULL,
  win.matrix = NULL,
  item.prior.var = NULL,
  lambda.initial = NULL,
  n.iter = 1000,
  hyperparameter = TRUE,
  chi = 0.01,
  psi = 0.01,
  burn.in = 100
) {
  if (is.null(win.matrix)) {
    # Create win matrix
    # get number of objects in study
    n.objects <- max(c(item1, item2))

    # Get winner and loser of each comparison
    winner <- ifelse(outcome == 1, item2, item1)
    loser <- ifelse(outcome == 0, item2, item1)

    # Turn each comparison (except ties) into a win/loss matrix
    win.matrix <- comparisons_to_matrix(n.objects, data.frame(winner, loser))
  } else {
    n.objects <- dim(win.matrix)[1]
  }

  # Get y_ij and n_ij (the number of times i beat j and i and
  # j were compared respectively)
  y <- win.matrix[lower.tri(win.matrix)]
  n <- (win.matrix + t(win.matrix))[lower.tri(win.matrix)]
  y <- n - y

  # Construct the design matrix
  X <- construct.design.matrix(n.objects)

  # Get inverse of prior covariance matrix
  # If not set, the prior is iid N(0,1^2)
  if (is.null(item.prior.var)) {
    item.prior.var <- 5^2 * diag(n.objects)
  }
  item.prior.var.inverse <- solve(item.prior.var)

  # Set initial values for lambda
  if (is.null(lambda.initial)) {
    lambda.initial <- numeric(n.objects)
  }
  if (n.objects != length(lambda.initial)) {
    stop(
      "Mismatch between number of objects in study and length of
       vector for initial estimates."
    )
  }

  # Remove pairs that were never compared
  non.zero.n <- length(n[n != 0])
  X <- X[which(n != 0), ]
  y <- y[which(n != 0)]
  n <- n[which(n != 0)]

  # Set up initial values
  lambda <- lambda.initial
  alpha.sq <- 1

  # Set up empty storage spaces
  lambda.matrix <- matrix(0, n.iter, n.objects)
  alpha.sq.vector <- numeric(n.iter)

  # Set commonly required constants
  unnormalised.mu <- Matrix::t(X) %*% (y - n / 2)
  grand.covariance <- sum(item.prior.var)

  # Set iteration counter and close when the function exits
  pb <- utils::txtProgressBar(min = 0, max = n.iter, style = 3)
  on.exit(close(pb), add = TRUE)

  # MCMC loop
  for (i in 1:n.iter) {
    if (hyperparameter == TRUE) {
      alpha.sq <- 1 /
        stats::rgamma(
          1,
          chi + n.objects / 2,
          0.5 * t(lambda) %*% item.prior.var.inverse %*% lambda + psi
        )
    }

    z <- BayesLogit::rpg(non.zero.n, n, as.numeric(X %*% lambda))
    Z <- Matrix::sparseMatrix(i = 1:non.zero.n, j = 1:non.zero.n, x = z)
    V <- base::chol2inv(base::chol(
      Matrix::t(X) %*% Z %*% X + item.prior.var.inverse / alpha.sq
    ))
    mu <- V %*% unnormalised.mu
    V.chol <- base::chol(V)
    lambda <- as.numeric(t(V.chol) %*% stats::rnorm(n.objects, 0, 1) + mu)

    # Translate quality parameters
    lambda <- lambda - mean(lambda)

    lambda.matrix[i, ] <- lambda
    alpha.sq.vector[i] <- alpha.sq
    pars.matrix <- cbind(lambda.matrix, alpha.sq.vector)
    utils::setTxtProgressBar(pb, i) # update text progress bar after each iter
  }
  close(pb)
  if (hyperparameter == TRUE) {
    mcmc_out <- coda::as.mcmc(
      x = pars.matrix[(burn.in + 1):n.iter, ],
      start = burn.in + 1,
      end = n.iter,
      thin = 1
    )
    coda::varnames(mcmc_out) <- c(
      paste0("lambda[", 1:n.objects, "]"),
      "alpha.sq"
    )
  } else {
    mcmc_out <- coda::as.mcmc(
      x = pars.matrix[(burn.in + 1):n.iter, 1:n.objects],
      start = burn.in + 1,
      end = n.iter,
      thin = 1
    )
    coda::varnames(mcmc_out) <- paste0("lambda[", 1:n.objects, "]")
  }
  return(mcmc_out)
}


#' Bayesian inference for the Bradley-Terry model with ties
#'
#' This function uses MCMC to sample from the posterior distribution of the
#' Bradley--Terry model with ties.A multivariate normal prior
#' distribution on the item quality parameters can be specified. An exponential
#' prior distribution is placed on the tie parameter theta, and a Metropolis-
#' Hasting random walk algorithm is used to update this parameter.
#'
#' @param n.objects The number of objects in the study.
#' @param outcome Vector of outcomes. 0 if item 1 is the winner,
#'  1 if item 2 is the winner, and 2 if it is a tie.
#' @param item1 Vector of first items.
#' @param item2 Vector of second items.
#' @param item.prior.var (optional) Matrix specifying the prior covariance of
#'  the item correlation parameters.
#' @param theta.initial (optional) Value of the tied parameter there for
#' the first MCMC iteration.
#' @param lambda.initial (optional) Vector containing the values of the
#'  item parameters for the first MCMC iteration.
#' @param n.iter Number of MCMC samples to be drawn.
#' @param hyperparameter Boolean indicating if inference should be performed
#'  for the prior variance hyperparameter. If TRUE the prior variance
#'  (main diagonal of the covariance matrix) must be set to 1.
#' @param psi (optional) Shape parameter for the inverse-gamma prior distribution on the
#' hyperparameter.
#' @param chi (optional) Rate parameter for the inverse-gamma prior distribution on the
#'  hyperparameter.
#' @param rw.sd (optional) Number describing the standard deviation of normal distribution
#' proposal distribution for theta.
#' @param theta.rate (optional) The rate parameter of the exponential prior
#' distribution placed on theta.
#' @param burn.in (optional) The number of iterations to use as a burn-in period. Default is 100.
#'
#' @details If `item.prior.var` is omitted, independent and identical
#' N(0, 5^2) prior distributions are placed on each object quality parameter.
#'
#' If `lambda.initial` is omitted, it is set to a vector of zeroes.
#'
#'
#' @return  A ["mcmc"][coda::mcmc] object containing samples from the posterior distribution
#'
#'
#' @examples
#' ############################################
#' ## Deprivation in Dar es Salaam, Tanzania ##
#' ## Seymour et al (2022)                   ##
#' ############################################
#' # Construct covariance matrix based on spatial informartion
#' sigma <- expm::expm(darEsSalaam$adjacencyMatrix)
#' sigma <- diag(diag(sigma)^-0.5) %*% sigma %*% diag(diag(sigma)^-0.5)
#'
#'
#' # Fit BT model with ties
#' # Using `n.iter = 3` here to reduce model runtime, you will need
#' # a larger number of iterations for valid inference.
#' darTiedModel <- BBTm.ties(
#'   n.objects = nrow(darEsSalaam$adjacencyMatrix),
#'   outcome = darEsSalaam$comparisons$outcome,
#'   item1 = darEsSalaam$comparisons$subward1,
#'   item2 = darEsSalaam$comparisons$subward2,
#'   item.prior.var = sigma,
#'   hyperparameter = TRUE, rw.sd = 0.005, n.iter = 3, burn.in = 0
#' )
#'
#' # Get posterior means
#' darTiedModelLambda <- parameter(darTiedModel, "lambda") - colMeans(parameter(darTiedModel, "lambda"))
#' lambda.mean <- rowMeans(darTiedModelLambda)
#'
#' # Generate trace plots
#' plot(lambda.mean)
#' plot(parameter(darTiedModel, "theta"), type = "l")
#' @export
#'
BBTm.ties <- function(
  n.objects,
  outcome,
  item1,
  item2,
  item.prior.var = NULL,
  theta.initial = NULL,
  lambda.initial = NULL,
  n.iter = 1000,
  hyperparameter = TRUE,
  chi = 0.01,
  psi = 0.01,
  rw.sd = 0.1,
  theta.rate = 0.01,
  burn.in = 100
) {
  # get number of objects in study
  n.objects <- max(c(item1, item2))

  if (n.iter <= burn.in) {
    stop(
      "Your burn in period is greater than the total number of iterations. ",
      "Please choose a shorter burn-in period or a larger number of total iterations."
    )
  }

  # Order pairs into winner/loser
  winner <- ifelse(
    outcome[outcome != 2] == 1,
    item2[outcome != 2],
    item1[outcome != 2]
  )
  loser <- ifelse(
    outcome[outcome != 2] == 0,
    item2[outcome != 2],
    item1[outcome != 2]
  )

  # Turn each comparison (except ties) into a win/loss matrix
  win.matrix <- comparisons_to_matrix(n.objects, data.frame(winner, loser))

  tie.matrix <- matrix(0, n.objects, n.objects)
  for (j in which(outcome == 2)) {
    tie.matrix[item1[j], item2[j]] <- tie.matrix[item1[j], item2[j]] + 1
    tie.matrix[item2[j], item1[j]] <- tie.matrix[item2[j], item1[j]] + 1
  }

  X <- construct.design.matrix.both.ways(n.objects)

  # Get inverse of prior covariance matrix
  if (is.null(item.prior.var)) {
    item.prior.var <- 5^2 * diag(n.objects)
  }
  item.prior.var.inverse <- solve(item.prior.var)

  # Set initial values for lambda
  if (is.null(lambda.initial)) {
    lambda.initial <- numeric(n.objects)
  }
  if (n.objects != length(lambda.initial)) {
    stop(
      "Mismatch between number of objects in study and length of vector for initial estimates."
    )
  }

  y <- c(
    t(win.matrix)[lower.tri(win.matrix)],
    win.matrix[lower.tri(win.matrix)]
  )
  t <- c(
    t(tie.matrix)[lower.tri(tie.matrix)],
    tie.matrix[lower.tri(tie.matrix)]
  )

  # Remove pairs of items that were never compared
  kappa <- (y + t) / 2
  non.zero.kappa <- length(kappa[kappa != 0])
  X <- X[which(kappa != 0), ]
  t <- t[which(kappa != 0)]
  y <- y[which(kappa != 0)]

  if (is.null(theta.initial)) {
    theta.initial <- 0.5
  }

  # Set constants for MCMC
  lambda <- lambda.initial
  theta <- theta.initial
  ones <- rep(1, non.zero.kappa)
  kappa <- (y + t) / 2
  alpha.sq <- 1
  grand.covariance <- sum(item.prior.var)

  # Create empty storage vessels
  lambda.matrix <- matrix(0, n.iter, n.objects) # store results
  theta.store <- numeric(n.iter) # store results
  alpha.sq.store <- numeric(n.iter) # store results

  pb <- utils::txtProgressBar(min = 0, max = n.iter, style = 3)
  on.exit(close(pb), add = TRUE)

  # MCMC
  for (i in 1:n.iter) {
    # Update alpha^2
    if (hyperparameter == TRUE) {
      alpha.sq <- 1 /
        stats::rgamma(
          1,
          0.01 + n.objects / 2,
          0.5 * t(lambda) %*% item.prior.var.inverse %*% lambda + 0.01
        )
    }

    # Update Z
    z <- BayesLogit::rpg(
      non.zero.kappa,
      y + t,
      as.numeric(X %*% lambda) - theta
    )
    Z <- Matrix::sparseMatrix(i = 1:non.zero.kappa, j = 1:non.zero.kappa, x = z)

    # Update lambda
    V <- chol2inv(chol(
      Matrix::t(X) %*% Z %*% X + item.prior.var.inverse / alpha.sq
    ))
    mu <- V %*% (Matrix::t(X) %*% (kappa + theta * Z %*% ones))
    V.chol <- chol(V)
    lambda <- as.numeric(t(V.chol) %*% stats::rnorm(n.objects, 0, 1) + mu)
    lambda <- lambda - mean(lambda) # translate to have mean 0

    # Update theta
    theta.prop <- theta + stats::rnorm(1, 0, rw.sd)
    if (theta.prop > 0) {
      loglike <- sum(t) /
        2 *
        log(exp(2 * theta) - 1) -
        sum((y + t) * (theta + log(1 + exp((X %*% lambda) - theta))))
      loglike.prop <- sum(t) /
        2 *
        log(exp(2 * theta.prop) - 1) -
        sum((y + t) * (theta.prop + log(1 + exp((X %*% lambda) - theta.prop))))
      log.p.acc <- loglike.prop -
        loglike +
        stats::dexp(theta.prop, theta.rate, log = TRUE) -
        stats::dexp(theta, theta.rate, log = TRUE)
      if (log(stats::runif(1)) < log.p.acc) {
        theta <- theta.prop
      }
    }

    theta.store[i] <- theta
    alpha.sq.store[i] <- alpha.sq
    lambda.matrix[i, ] <- as.numeric(lambda)
    utils::setTxtProgressBar(pb, i) # update text progress bar after each iter
  }
  pars.matrix <- cbind(lambda.matrix, theta.store, alpha.sq.store)

  if (hyperparameter == TRUE) {
    mcmc_out <- coda::as.mcmc(
      x = pars.matrix[(burn.in + 1):n.iter, 1:(n.objects + 2)],
      start = burn.in + 1,
      end = n.iter,
      thin = 1
    )
    coda::varnames(mcmc_out) <- c(
      paste("lambda[", 1:n.objects, "]", sep = ""),
      "theta",
      "alpha.sq"
    )
  } else {
    mcmc_out <- coda::as.mcmc(
      x = pars.matrix[(burn.in + 1):n.iter, 1:(n.objects + 1)],
      start = burn.in + 1,
      end = n.iter,
      thin = 1
    )
    coda::varnames(mcmc_out) <- c(
      paste0("lambda[", 1:n.objects, "]"),
      "theta"
    )
  }
  return(mcmc_out)
}


#' Bayesian Bradley--Terry model with comparison-specific effect
#'
#' This function fits the Bradley-Terry model with a comparison specific effect.
#' Each comparison can be assigned a real value to allow for a specific effect
#' for the comparison, such as bias, ordering or home/away effect. The value of
#' this effect is denoted $kappa$. The function places a normal prior distribution
#' on both kappa and the item quality parameters lambda.
#'
#'
#'
#' @param outcome Vector of outcomes. 1 if item2 is the winner,
#'  0 if item1 is the winner.
#' @param item1 Vector of first items.
#' @param item2 Vector of second items.
#' @param item.prior.var (optional) Matrix specifying the prior covariance of
#'  the item correlation parameters.
#' @param lambda.initial (optional) Vector containing the values of the
#'  item parameters for the first MCMC iteration.
#' @param n.iter Number of MCMC samples to be drawn.
#' @param advantage (optional) A vector with the value of the comparisons specific
#'  effect for each comparison.
#' @param kappa.initial (optional) An initial value for the comparison specific
#'  value kappa.
#'  @param kappa.var (optional) The prior variance of the comparison specific
#'  value kappa.
#' @param hyperparameter Boolean indicating if inference should be performed
#'  for the prior variance hyperparameter. If TRUE the prior variance
#'  (main diagonal of the covariance matrix) must be set to 1.
#' @param psi Shape parameter for the inverse-gamma prior distribution on the
#' hyperparameter.
#' @param chi Rate parameter for the inverse-gamma prior distribution on the
#'  hyperparameter.
#' @param burn.in Number of iterations to use as a burn-in period. Default is 100.
#'
#' @details If `item.prior.var` is omitted, independent and identical
#' N(0, 5^2) prior distributions are placed on each object quality parameter.
#'
#' If `lambda.initial` is omitted, it is set to a vector of zeroes.
#'
#' If `lambda.var` is omitted, it is set to N(0, 5^2).
#'
#' #' If `kappa.var` is omitted, it is set to N(0, 5^2), if `kappa.initial` is omitted
#' it is set to 0.5.
#'
#' @return  A ["mcmc"][coda::mcmc] object containing samples from the posterior distribution
#'
#' @keywords internal
#'
#' @export
#'
BBTm.no.formula <- function(
  outcome,
  item1,
  item2,
  item.prior.var,
  lambda.initial,
  advantage = NULL,
  kappa.initial = NULL,
  kappa.var = NULL,
  n.iter = 1000,
  hyperparameter = TRUE,
  chi = 0.01,
  psi = 0.01,
  burn.in = 100
) {
  # get number of objects in study
  n.objects <- max(c(item1, item2))

  # get number of comparisons
  n.comp <- length(outcome)

  # Get winner and loser of each comparison
  winner <- ifelse(outcome == 1, item2, item1)
  loser <- ifelse(outcome == 0, item2, item1)

  # Get y_ij
  y <- outcome
  k <- y - 0.5

  # Construct the design matrix
  X <- construct.design.matrix.by.comparison(item1, item2)

  # Get inverse of prior covariance matrix
  # If not set, the prior is iid N(0, 5^2)
  if (is.null(item.prior.var)) {
    item.prior.var <- 5^2 * diag(n.objects)
  }
  item.prior.var.inverse <- solve(item.prior.var)

  # Set initial values for lambda
  if (is.null(lambda.initial)) {
    lambda.initial <- numeric(n.objects)
  }
  if (n.objects != length(lambda.initial)) {
    stop(
      "Mismatch between number of objects in study and length of
       vector for initial estimates."
    )
  }

  # Set up initial values
  lambda <- lambda.initial
  alpha.sq <- 1

  # Determine if inference is required for advantages
  if (is.null(advantage)) {
    kappa <- 0
    advantage <- rep(0, n.comp)
    advantage.inf <- FALSE
  } else {
    if (is.null(kappa.initial)) {
      kappa <- 0.5
    } else {
      kappa <- kappa.initial
    }

    if (is.null(kappa.var)) {
      kappa.var <- 5^2
    }
    kappa.precision <- 1 / kappa.var
    kappa.vector <- numeric(n.iter)
    advantage.inf <- TRUE
  }

  # Set up empty storage spaces
  lambda.matrix <- matrix(0, n.iter, n.objects)
  alpha.sq.vector <- numeric(n.iter)

  # Set commonly required constants
  grand.covariance <- sum(item.prior.var)

  # Set iteration counter
  pb <- utils::txtProgressBar(min = 0, max = n.iter, style = 3)
  on.exit(close(pb), add = TRUE)

  # MCMC loop
  for (i in 1:n.iter) {
    if (hyperparameter == TRUE) {
      alpha.sq <- 1 /
        stats::rgamma(
          1,
          chi + n.objects / 2,
          0.5 * t(lambda) %*% item.prior.var.inverse %*% lambda + psi
        )
    }
    # Update Z
    z <- BayesLogit::rpg(
      n.comp,
      1,
      as.numeric(X %*% lambda) +
        kappa * advantage
    )
    Z <- Matrix::sparseMatrix(i = 1:n.comp, j = 1:n.comp, x = z)

    # Update lambda
    V <- chol2inv(chol(
      Matrix::t(X) %*% Z %*% X + item.prior.var.inverse / alpha.sq
    ))
    mu <- V %*% (Matrix::t(X) %*% (k - kappa * Z %*% advantage))
    V.chol <- chol(V)
    lambda <- as.numeric(t(V.chol) %*% stats::rnorm(n.objects, 0, 1) + mu)

    # Update kappa
    if (advantage.inf) {
      S <- (t(advantage) %*% Z %*% advantage + kappa.precision)^-1
      mu.kappa <- S * t(advantage) %*% (k - Z %*% X %*% lambda)
      kappa <- stats::rnorm(1, as.numeric(mu.kappa), as.numeric(S))
      kappa.vector[i] <- kappa
    }

    # Translate quality parameters
    lambda <- lambda - mean(lambda)

    lambda.matrix[i, ] <- lambda
    alpha.sq.vector[i] <- alpha.sq

    utils::setTxtProgressBar(pb, i) # update text progress bar after each iter
  }
  close(pb)
  if (hyperparameter == TRUE & advantage.inf == TRUE) {
    # Output alpha.sq and kappa
    pars.matrix <- cbind(lambda.matrix, alpha.sq.vector, kappa.vector)
    mcmc_out <- coda::as.mcmc(
      x = pars.matrix[(burn.in + 1):n.iter, ],
      start = burn.in + 1,
      end = n.iter,
      thin = 1
    )
    coda::varnames(mcmc_out) <- c(
      paste0("lambda[", 1:n.objects, "]"),
      "alpha.sq",
      "kappa"
    )
  } else if (hyperparameter == FALSE & advantage.inf == TRUE) {
    pars.matrix <- cbind(lambda.matrix, kappa.vector)
    # Output only kappa
    mcmc_out <- coda::as.mcmc(
      x = pars.matrix[(burn.in + 1):n.iter, ],
      start = burn.in + 1,
      end = n.iter,
      thin = 1
    )
    coda::varnames(mcmc_out) <- c(
      paste0("lambda[", 1:n.objects, "]"),
      "kappa"
    )
  } else if (hyperparameter == FALSE & advantage.inf == FALSE) {
    pars.matrix <- cbind(lambda.matrix, alpha.sq.vector)
    # Output only alpha.sq
    mcmc_out <- coda::as.mcmc(
      x = pars.matrix[(burn.in + 1):n.iter, ],
      start = burn.in + 1,
      end = n.iter,
      thin = 1
    )
    coda::varnames(mcmc_out) <- c(
      paste0("lambda[", 1:n.objects, "]"),
      "alpha.sq"
    )
  } else {
    pars.matrix <- lambda.matrix
    mcmc_out <- coda::as.mcmc(
      x = pars.matrix[(burn.in + 1):n.iter, ],
      start = burn.in + 1,
      end = n.iter,
      thin = 1
    )
    coda::varnames(mcmc_out) <- paste0("lambda[", 1:n.objects, "]")
  }
  return(mcmc_out)
}


#' Bayesian Bradley--Terry model with comparison- and item-specific effect and formula
#'
#' This function fits the Bradley-Terry model with comparison  and item
#' specific effects. Each comparison can be assigned a real value to allow for a
#' specific effect for the comparison, such as bias, ordering or home/away effect.
#' The value of this effect is denoted kappa. The item specific effects are
#' described through a formula and data.frame containing the value. The function
#' places a normal prior distribution on both kappa and the item specific
#' parameters beta.
#'
#'
#'
#' @param outcome Vector of outcomes. 1 if item2 is the winner,
#'  0 if item1 is the winner.
#' @param item1 Vector of first items.
#' @param item2 Vector of second items.
#' @param formula Formula with no left-hand-side specifying the item specific
#' effects.
#' @param data Data frame with a row corresponding to each player and a column corresponding
#' to each covariate.
#' @param item.prior.var (optional) Matrix specifying the prior covariance of
#'  the item correlation parameters.
#' @param beta.initial (optional) Vector containing the values of the
#'  item specific  parameters for the first MCMC iteration.
#' @param n.iter Number of MCMC samples to be drawn.
#' @param advantage (optional) A vector with the value of the comparisons specific
#'  effect for each comparison.
#' @param kappa.initial (optional) An initial value for the comparison specific
#'  value kappa.
#' @param kappa.var (optional) The prior variance of the comparison specific
#'  value kappa.
#' @param hyperparameter Boolean indicating if inference should be performed
#'  for the prior variance hyperparameter. If TRUE the prior variance
#'  (main diagonal of the covariance matrix) must be set to 1.
#' @param psi Shape parameter for the inverse-gamma prior distribution on the
#' hyperparameter.
#' @param chi Rate parameter for the inverse-gamma prior distribution on the
#'  hyperparameter.
#' @param burn.in The number of iterations to use for a burn.in, default is 100.
#'
#' @details If `item.prior.var` is omitted, independent and identical
#' N(0, 5^2) prior distributions are placed on each object quality parameter.
#'
#' If `beta.initial`is omitted, it is set to a vector of zeroes.
#'
#' If `kappa.var` is omitted, it is set to N(0, 5^2), if `kappa.initial` is omitted
#' it is set to 0.5.
#'
#'
#' @return  A ["mcmc"][coda::mcmc] object containing samples from the posterior distribution.
#'
#' @keywords internal
#'
#' @export
#'
BBTm.with.formula <- function(
  outcome,
  item1,
  item2,
  formula = NULL,
  data = NULL,
  advantage = NULL,
  kappa.initial = NULL,
  kappa.var = NULL,
  item.prior.var = NULL,
  beta.initial = NULL,
  n.iter = 1000,
  hyperparameter = TRUE,
  chi = 0.01,
  psi = 0.01,
  burn.in = 100
) {
  # get number of objects in study
  n.objects <- max(c(item1, item2))
  n.comp <- length(outcome)

  # Get y_ij
  y <- 1 - outcome
  k <- y - 0.5

  # Construct the design matrix
  X <- construct.generalised.design.matrix(item1, item2, formula, data)
  formula.model <- stats::model.frame(formula, data)

  # Get inverse of prior covariance matrix
  if (is.null(item.prior.var) & hyperparameter == FALSE) {
    item.prior.var <- 5^2 * diag(dim(X)[2]) # fix prior to be N( 0, 5^2)
  } else if (is.null(item.prior.var)) {
    item.prior.var <- diag(dim(X)[2])
  }
  item.prior.var.inverse <- solve(item.prior.var)

  # Determine if inference is required for advantages
  if (is.null(advantage)) {
    kappa <- 0
    advantage <- rep(0, n.comp)
    advantage.inf <- FALSE
  } else {
    if (is.null(kappa.initial)) {
      kappa <- 0.5
    } else {
      kappa <- kappa.initial
    }

    if (is.null(kappa.var)) {
      kappa.var <- 5^2
    }

    kappa.precision <- 1 / kappa.var
    kappa.vector <- numeric(n.iter)
    advantage.inf <- TRUE
  }
  n.betas <- dim(X)[2]
  # Set initial values for beta
  if (is.null(beta.initial)) {
    beta.initial <- numeric(n.betas)
  }
  if (n.betas != length(beta.initial)) {
    stop(
      "Mismatch between number of covariates in study and length of vector for initial estimates."
    )
  }

  # Set up MCMC
  beta <- beta.initial
  alpha.sq <- 1

  beta.matrix <- matrix(0, n.iter, n.betas)
  lambda.matrix <- matrix(0, n.iter, n.objects)
  alpha.sq.vector <- numeric(n.iter)
  grand.covariance <- sum(item.prior.var)

  pb <- utils::txtProgressBar(min = 0, max = n.iter, style = 3)
  on.exit(close(pb), add = TRUE)
  for (i in 1:n.iter) {
    if (hyperparameter == TRUE) {
      alpha.sq <- 1 /
        stats::rgamma(
          1,
          chi + dim(X)[2] / 2,
          0.5 * t(beta) %*% item.prior.var.inverse %*% beta + psi
        )
    }

    z <- BayesLogit::rpg(
      n.comp,
      1,
      as.numeric(X %*% beta) +
        kappa * advantage
    )
    Z <- Matrix::sparseMatrix(i = 1:n.comp, j = 1:n.comp, x = z)
    V <- base::chol2inv(base::chol(
      Matrix::t(X) %*% Z %*% X + item.prior.var.inverse / alpha.sq
    ))
    mu <- V %*% (Matrix::t(X) %*% (k - kappa * Z %*% advantage))
    V.chol <- base::chol(V)
    beta <- as.numeric(t(V.chol) %*% stats::rnorm(dim(X)[2], 0, 1) + mu)

    # Update kappa
    if (advantage.inf) {
      S <- (t(advantage) %*% Z %*% advantage + kappa.precision)^-1
      mu.kappa <- S * t(advantage) %*% (k - Z %*% X %*% beta)
      kappa <- stats::rnorm(1, as.numeric(mu.kappa), as.numeric(S))
      kappa.vector[i] <- kappa
    }

    # Translate quality parameters
    lambda <- as.matrix(formula.model) %*% beta
    lambda <- lambda - mean(lambda)

    beta.matrix[i, ] <- t(beta)
    lambda.matrix[i, ] <- lambda
    alpha.sq.vector[i] <- alpha.sq
    utils::setTxtProgressBar(pb, i) # update text progress bar after each iter
  }
  close(pb)
  if (hyperparameter == TRUE & advantage.inf == TRUE) {
    # Output alpha.sq and kappa
    pars.matrix <- cbind(
      beta.matrix,
      lambda.matrix,
      kappa.vector,
      alpha.sq.vector
    )
    # Output only kappa
    mcmc_out <- coda::as.mcmc(
      x = pars.matrix[(burn.in + 1):n.iter, ],
      start = burn.in + 1,
      end = n.iter,
      thin = 1
    )
    coda::varnames(mcmc_out) <- c(
      paste0("beta[", 1:n.betas, "]"),
      paste0("lambda[", 1:n.objects, "]"),
      "kappa",
      "alpha.sq"
    )
  } else if (hyperparameter == FALSE & advantage.inf == TRUE) {
    # Output only kappa
    pars.matrix <- cbind(beta.matrix, lambda.matrix, kappa.vector)
    # Output only kappa
    mcmc_out <- coda::as.mcmc(
      x = pars.matrix[(burn.in + 1):n.iter, ],
      start = burn.in + 1,
      end = n.iter,
      thin = 1
    )
    coda::varnames(mcmc_out) <- c(
      paste0("beta[", 1:n.betas, "]"),
      paste0("lambda[", 1:n.objects, "]"),
      "kappa"
    )
  } else if (hyperparameter == FALSE & advantage.inf == FALSE) {
    # Output only alpha.sq

    pars.matrix <- cbind(beta.matrix, lambda.matrix, alpha.sq.vector)
    # Output only kappa
    mcmc_out <- coda::as.mcmc(
      x = pars.matrix[(burn.in + 1):n.iter, ],
      start = burn.in + 1,
      end = n.iter,
      thin = 1
    )
    coda::varnames(mcmc_out) <- c(
      paste0("lambda[", 1:n.objects, "]"),
      "alpha.sq"
    )
  } else {
    pars.matrix <- cbind(beta.matrix, lambda.matrix)
    # Output only kappa
    mcmc_out <- coda::as.mcmc(
      x = pars.matrix[(burn.in + 1):n.iter, ],
      start = burn.in + 1,
      end = n.iter,
      thin = 1
    )
    coda::varnames(mcmc_out) <- c(
      paste0("beta[", 1:n.betas, "]"),
      paste0("lambda[", 1:n.objects, "]")
    )
  }
  return(mcmc_out)
}


#' Generalised Bradley-Terry model
#'
#' This function fits the Bradley-Terry model with comparison  and item
#' specific effects. Each comparison can be assigned a real value to allow for a
#' specific effect for the comparison, such as bias, ordering or home/away effect.
#' The value of this effect is denoted kappa. The item specific effects are
#' described through a formula and data.frame containing the value. The function
#' places a normal prior distribution on both kappa and the item specific
#' parameters beta.
#'
#'
#' @inheritParams BBTm.with.formula
#'
#' @param lambda.initial (optional) Vector containing the values of the
#'  item parameters for the first MCMC iteration.
#'
#'
#'
#' @details If `item.prior.var` is omitted, independent and identical
#' N(0, 5^2) prior distributions are placed on each object quality parameter.
#'
#' If `beta.initial`is omitted, it is set to a vector of zeroes.
#'
#' If `kappa.var` is omitted, it is set to N(0, 5^2), if `kappa.initial` is omitted
#' it is set to 0.5.
#'
#'
#' @return  A ["mcmc"][coda::mcmc] object containing samples from the posterior distribution.
#'
#'
#'
#' @examples
#' \donttest{
#' #####################
#' ## Wimbledon 2019 ##
#' ####################
#' # Fit model where the quality of each player depends on their rank
#' # and the number of points they had immediately before the tournament.
#' # Allow an effect for a match being in the first or second week.
#' wimbledonModel <- BBTm(
#'   outcome = wimbledon$matches$outcome,
#'   item2 = wimbledon$matches$loser,
#'   item1 = wimbledon$matches$winner,
#'   advantage = wimbledon$matches$secondWeek,
#'   formula = ~ rank + points,
#'   data = wimbledon$players,
#'   n.iter = 1000
#' )
#'
#' # Plot posterior distributions
#' hist(parameter(wimbledonModel, "kappa"), main = "", xlab = expression(kappa), freq = FALSE)
#' }
#' @export
#'
BBTm <- function(
  outcome,
  item1,
  item2,
  lambda.initial = NULL,
  item.prior.var = NULL,
  beta.initial = NULL,
  n.iter = 1000,
  formula = NULL,
  data = NULL,
  advantage = NULL,
  kappa.initial = NULL,
  kappa.var = NULL,
  hyperparameter = TRUE,
  chi = 0.01,
  psi = 0.01,
  burn.in = 100
) {
  if (!is.null(lambda.initial) & !is.null(beta.initial)) {
    stop("Cannot set initial values for both lambda and beta")
  }

  if (!is.null(formula)) {
    output <- BBTm.with.formula(
      outcome,
      item1,
      item2,
      formula,
      data,
      advantage,
      kappa.initial,
      kappa.var,
      item.prior.var,
      beta.initial,
      n.iter,
      hyperparameter,
      chi,
      psi,
      burn.in
    )
  } else {
    output <- BBTm.no.formula(
      outcome,
      item1,
      item2,
      item.prior.var,
      lambda.initial,
      advantage,
      kappa.initial,
      kappa.var,
      n.iter,
      hyperparameter,
      chi,
      psi,
      burn.in
    )
  }
  return(output)
}
