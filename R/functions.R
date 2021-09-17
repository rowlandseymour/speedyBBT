#' Construct the Bradley--Terry design matrix
#'
#'
#' @param N the number of objects
#' @return design matrix X
#'
#' @keywords internal
#'
#' @examples
#'
#' #design matrix with 3 objects
#'
#' X <- construct.design.matrix(3)
#'
#' @export
construct.design.matrix <- function(N){

  all.pairs  <- t(utils::combn(N, 2))
  winners    <- Matrix::sparseMatrix(i = 1:(N*(N-1)/2), j = all.pairs[, 1], x = 1, dims = c(N*(N-1)/2, N))
  losers     <- Matrix::sparseMatrix(i = 1:(N*(N-1)/2), j = all.pairs[, 2], x = -1)
  X <- winners + losers

  return(X)
}

#' Construct the Bradley--Terry design matrix for ties
#' This is the design matrix for tied comparisons. Each permutation (rather than combination) is featured, i.e. both (i, j) and (j, i).
#'
#' @param N the number of objects
#' @return design matrix X
#'
#' @keywords internal
#'
#' @examples
#'
#' #design matrix with 3 objects
#'
#' X <- construct.design.matrix.both.ways(3)
#'
#' @export
construct.design.matrix.both.ways <- function (N) {
  all.pairs <- t(utils::combn(N, 2))
  a <- c(all.pairs[, 1], all.pairs[, 2])
  b <- c(all.pairs[, 2], all.pairs[, 1])
  all.pairs <- cbind(a, b)
  winners <- Matrix::sparseMatrix(i = 1:(N * (N - 1)), j = all.pairs[, 1], x = 1, dims = c(N^2 - N, N))
  losers <- Matrix::sparseMatrix(i = 1:(N * (N - 1)), j = all.pairs[, 2], x = -1, dims = c(N^2 - N, N))
  X <- winners + losers
  return(X)
}

#' Gibbs sampler for Bradley--Terry
#'
#' @param N the number of objects
#' @param y a vector containing the number of times each object beat another
#' @param n a vector containing the the number of times each pair was compared
#' @param X optional design matrix. If one is not supplied, a generic Bradley-Terry design matrix is used
#' @param prior.sigma the prior covariance matrix
#' @param prior.sigma.inverse the inverse of the prior covariance matrix (optional)
#' @param lambda.initial a vector containing the initial value for each object
#' @param n.iter the number of samples to be drawn
#' @param hyperparameter boolean indicating if inference should be performed for the prior variance hyperparameter. If TRUE the prior variance (main diagonal of the covariance matrix) must be set to 1.
#'
#' @return  A data frame containing samples from the posterior distribution
#'
#' @export
#'
gibbs.bt <- function(N, y, n, X, prior.sigma, lambda.initial, n.iter = 200, hyperparameter = FALSE, prior.sigma.inverse){

  if(missing(X))
    X <- construct.design.matrix(N) #If no design matrix is specified, then one is created

  if(missing(prior.sigma.inverse))
    prior.sigma.inverse <- solve(prior.sigma) #if inverse is not given, then compute it


  non.zero.n       <- length(n[n!=0])
  X                <- X[which(n!=0), ]
  y                <- y[which(n!=0)]
  n                <- n[which(n!=0)]
  lambda           <- lambda.initial
  lambda.matrix    <- matrix(0, n.iter, N)  #store results
  alpha.sq.vector  <- numeric(n.iter)
  Lambda.vector  <- numeric(n.iter)
  unnormalised.mu  <- Matrix::t(X)%*%(y - n/2)
  alpha.sq         <- 1
  grand.covariance <- sum(prior.sigma)
  pb <- utils::txtProgressBar(min = 0, max = n.iter, style = 3)
  for(i in 1:n.iter){

    if(hyperparameter == TRUE)
      alpha.sq     <- 1/stats::rgamma(1, 0.01 + N/2, 0.5*t(lambda)%*%prior.sigma.inverse%*%lambda + 0.01)

    omega        <- BayesLogit::rpg(non.zero.n, n, as.numeric(X%*%lambda))
    d.omega      <- Matrix::sparseMatrix(i = 1:non.zero.n, j = 1:non.zero.n, x = omega)
    V            <- base::chol2inv(base::chol(Matrix::t(X)%*%d.omega%*%X + prior.sigma.inverse/alpha.sq))
    mu           <- V%*%unnormalised.mu
    V.chol       <- base::chol(V)
    lambda       <- as.numeric(t(V.chol)%*%stats::rnorm(N, 0, 1) + mu)

    #Translate quality parameters
    Lambda       <- rnorm(1, 0, sqrt(grand.covariance))
    lambda       <- lambda - mean(lambda) + Lambda

    lambda.matrix[i, ] <- lambda
    alpha.sq.vector[i] <- alpha.sq
    Lambda.vector[i]   <- Lambda
    utils::setTxtProgressBar(pb, i) # update text progress bar after each iter
  }
  if(hyperparameter == TRUE)
    return(list("lambda" = lambda.matrix, "alpha.sq" = alpha.sq.vector, "Lambda" = Lambda.vector))
  else
    return(list("lambda" = lambda.matrix, "Lambda" = Lambda.vector))
}

#' Gibbs sampler for Bradley--Terry with ties
#'
#' @param N the number of objects
#' @param y a vector containing the number of times each object beat another (this must contain all permutations of objects)
#' @param t a vector containing the number of times each pair of objects tied
#' @param n a vector containing the the number of times each pair was compared
#' @param X optional design matrix. If one is not supplied, a generic Bradley-Terry design matrix is used
#' @param prior.sigma the prior covariance matrix
#' @param prior.sigma.inverse the inverse of the prior covariance matrix (optional)
#' @param lambda.initial a vector containing the initial value for each object
#' @param n.iter the number of samples to be drawn
#' @param hyperparameter boolean indicating if inference should be performed for the prior variance hyperparameter. If TRUE the prior variance (main diagonal of the covariance matrix) must be set to 1.
#' @param rw.sd The standard deviation of the random walk for delta
#'
#' @return  A data frame containing samples from the posterior distribution
#'
#' @export
#'

gibbs.bt.with.ties <- function(N, y, t, n, X, prior.sigma, theta.precision, lambda.initial, n.iter = 200, hyperparameter = FALSE, prior.sigma.inverse, rw.sd = 0.1){

  if(missing(X))
    X <- construct.design.matrix.both.ways(N) #If no design matrix is specified, then one is created

  if(missing(prior.sigma.inverse))
    prior.sigma.inverse <- solve(prior.sigma) #if inverse is not given, then compute it


kappa           <- (y + t)/2
non.zero.kappa <- length(kappa[kappa != 0])
X          <- X[which(kappa != 0), ]
t          <- t[which(kappa != 0)]
y          <- y[which(kappa != 0)]
ones       <- rep(1, non.zero.kappa)
kappa      <- (y + t)/2


#Prepare for MCMC
lambda             <- lambda.initial
delta              <- 0
grand.covariance   <- sum(prior.sigma)
lambda.matrix      <- matrix(0, N, n.iter)  #store results
delta.store        <- numeric(n.iter)  #store results
Lambda.store       <- numeric(n.iter)  #store results
alpha.sq.store     <- numeric(n.iter)  #store results
alpha.sq           <- 1
pb                 <- utils::txtProgressBar(min = 0, max = n.iter, style = 3)


loglike <- sum(t)*log(exp(2*delta) - 1) + sum((y+t)*(-delta - log(1 + exp(as.numeric(X%*%lambda) - delta))))


#MCMC
for(i in 1:n.iter){

  if(hyperparameter == TRUE)
    alpha.sq     <- 1/stats::rgamma(1, 0.01 + N/2, 0.5*t(lambda)%*%prior.sigma.inverse%*%lambda + 0.01)

  #Update Z
  z              <- BayesLogit::rpg(non.zero.kappa, y + t, as.numeric(X%*%lambda) - delta)
  Z              <- Matrix::sparseMatrix(i = 1:non.zero.kappa, j = 1:non.zero.kappa, x = z)

  #Update lambda
  V              <- chol2inv(chol(Matrix::t(X)%*%Z%*%X + prior.sigma.inverse/alpha.sq))
  mu             <- V%*%(Matrix::t(X)%*%(kappa + delta*Z%*%ones))
  V.chol         <- chol(V)
  lambda         <- as.numeric(t(V.chol)%*%stats::rnorm(N, 0, 1) + mu)

  #Translate quality parameters
  Lambda       <- rnorm(1, 0, sqrt(grand.covariance)/N)
  lambda       <- lambda - mean(lambda) + Lambda

  #Update delta
  delta.prop   <- delta + rnorm(1, 0, rw.sd)
  if(delta.prop > 0){
    loglike      <- sum(t)/2*log(exp(2*delta) - 1)  - sum((y+t)*(delta + log(1 + exp((X%*%lambda) - delta))))
    loglike.prop <- sum(t)/2*log(exp(2*delta.prop) - 1) - sum((y+t)*(delta.prop + log(1 + exp((X%*%lambda) - delta.prop))))
    log.p.acc    <- loglike.prop - loglike + dexp(delta.prop, 0.01, log = TRUE) - dexp(delta, 0.01, log = TRUE)
    if(log(runif(1)) < log.p.acc)
      delta      <- delta.prop
  }



  delta.store[i]     <- delta
  alpha.sq.store[i]  <- alpha.sq
  Lambda.store[i]    <- Lambda
  lambda.matrix[, i] <- as.numeric(lambda)
  utils::setTxtProgressBar(pb, i) # update text progress bar after each iter
}

if(hyperparameter == TRUE)
  return(list("lambda" = lambda.matrix, "delta" = delta.store, "alpha.sq" = alpha.sq.store, "Lambda" = Lambda.store))
else
  return(list("lambda" = lambda.matrix, "delta" = delta.store, "Lambda" = Lambda.store))

}
