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
  X <- matrix(0, 0, N)
  for(i in 1:(N-1)){

    neg.one.diag <- -1*diag(N-i)
    ones          <- matrix(1, N-i, 1)
    zeros         <- matrix(0, N-i, i - 1)

    #Add to end of design matrix
    temp <- cbind(zeros, ones)
    temp <- cbind(temp, neg.one.diag)
    X <- rbind(X, temp)
  }
  X <- Matrix::Matrix(X, sparse = T)
  return(X)
}

#' Gibbs sampler for Bradley--Terry
#'
#' @param N the number of objects
#' @param y a vector containing the number of times each object beat another
#' @param n a vector containing the the number of times each pair was compared
#' @param X optional design matrix. If one is not supplied, a generic Bradley-Terry deisgn matrix is used
#' @param prior.sigma.inverse the inverse of the prior covariance matrix
#' @param lamabda.initial a vector containing the initial value for each object
#' @param n.iter the number of samples to be drawn
#' @param hyperparameter boolean indicating if inference should be performed for the prior variance hyperparameter. If TRUE the prior variance (main diagonal of the covariance matrix) must be set to 1.
#'
#' @return  A data frame containing samples from the posterior distribution
#'
#' @export
#'
gibbs.bt <- function(N, y, n, X, prior.sigma.inverse, lambda.initial, n.iter = 200, hyperparameter = FALSE){

  if(missing(X))
    X <- construct.design.matrix(N) #If no design matrix is specified, then one is created

  non.zero.n       <- length(n[n!=0])
  X                <- X[which(n!=0), ]
  y                <- y[which(n!=0)]
  n                <- n[which(n!=0)]
  lambda           <- lambda.initial
  lambda.matrix    <- matrix(0, n.iter, N)  #store results
  alpha.sq.vector  <- numeric(n.iter)
  unnormalised.mu  <- Matrix::t(X)%*%(y - n/2)
  alpha.sq         <- 1
  pb <- txtProgressBar(min = 0, max = n.iter, style = 3)
  for(i in 1:n.iter){

    if(hyperparameter == TRUE)
      alpha.sq     <- 1/stats::rgamma(1, 0.01 + N/2, 0.5*t(lambda)%*%prior.sigma.inverse%*%lambda + 0.01)

    omega        <- rpg(non.zero.n, n, as.numeric(X%*%lambda))
    d.omega      <- Matrix::sparseMatrix(i = 1:non.zero.n, j = 1:non.zero.n, x = omega)
    V            <- base::chol2inv(base::chol(Matrix::t(X)%*%d.omega%*%X + prior.sigma.inverse/alpha.sq))
    mu           <- V%*%unnormalised.mu
    V.chol       <- base::chol(V)
    lambda       <- t(V.chol)%*%rnorm(N, 0, 1) + mu
    lambda       <- as.numeric(lambda - mean(as.numeric(lambda)))

    lambda.matrix[i, ] <- lambda
    alpha.sq.vector[i] <- alpha.sq
    setTxtProgressBar(pb, i) # update text progress bar after each iter
  }
  if(hyperparameter == TRUE)
    return(list("lambda" = lambda.matrix, "alpha.sq" = alpha.sq.vector))
  else
    return(lambda.matrix)
}
