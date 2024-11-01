#' Construct Win Matrix from Comparisons
#'
#' This function constructs a win matrix from a data frame of comparisons. It is needed for the MCMC functions.
#'
#' @param n.objects The number of areas in the study.
#' @param comparisons An N x 2 data frame, where N is the number of comparisons. Each row should correspond to a judgment. The first column is the winning object, the second column is the more losing object. The areas should be labeled from 1 to n.objects.
#' @return A matrix where the i, j th element is the number of times object i beat object j.
#'
#' @examples
#'
#' #Generate some sample comparisons
#' comparisons <- data.frame("winner" = c(1, 3, 2, 2), "loser" = c(3, 1, 1, 3))
#'
#' #Create matrix from comparisons
#' win.matrix <- comparisons_to_matrix(3, comparisons)
#'
#' @export
comparisons_to_matrix <- function(n.objects, comparisons){

  win.matrix <- matrix(0, n.objects, n.objects) #construct empty matrix

  for(j in 1:dim(comparisons)[1]) #for each comparisons, enter outcome into win matrix
    win.matrix[comparisons[j, 2], comparisons[j, 1]] <- win.matrix[comparisons[j, 2], comparisons[j, 1]] + 1


  return(win.matrix)
}


#' Construct the Bradley--Terry design matrix
#'
#'
#' @param n.objects the number of objects
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
#' @keywords internal
#'
#' @export
construct.design.matrix <- function(n.objects){

  all.pairs  <- t(utils::combn(n.objects, 2))
  winners    <- Matrix::sparseMatrix(i = 1:(n.objects*(n.objects-1)/2),
                                     j = all.pairs[, 1], x = 1,
                                     dims = c(n.objects*(n.objects-1)/2, n.objects))
  losers     <- Matrix::sparseMatrix(i = 1:(n.objects*(n.objects-1)/2),
                                     j = all.pairs[, 2], x = -1)
  X <- winners + losers

  return(X)
}


#' Construct the generalised Bradley--Terry design matrix
#'
#'
#' @param player1 vector containing the labels of player1
#' @param player2 vector containing the labels of player2
#' @param formula right-hand-side only formula for the player specific covariates
#' @param data dataframe containing the values of the player specific covariates
#' @return design matrix X
#'
#' @keywords internal
#'
#' @examples
#'
#' #Generate data.frame
#' example.df <- data.frame("a" = stats::runif(100, 0, 3),
#'                     "b" = sample(1:2, 100, TRUE))
#'
#' #Generate formula
#' example.formula <- ~  a + b + I(a^2)
#'
#' #Generate comparisons of three players
#' player1 <- c(1, 3, 2, 1)
#' player2 <- c(3, 1, 1, 2)
#' X <- construct.generalised.design.matrix(player1, player2, example.formula, example.df)
#'
#' @keywords internal
#'
#' @export
construct.generalised.design.matrix <- function(player1, player2, formula, data){

  #Get data frame from formula
  formula.model <- stats::model.frame(formula, data)

  n.objects <- dim(data)[1]
  K <- length(player1)
  all.pairs  <- t(utils::combn(n.objects, 2))

  X <- as.matrix(formula.model[player1, ] - formula.model[player2, ]) #Computer lambda_i - lambda_j
  dimnames(X) <- c(NULL, NULL) #Remove dimension names

  return(X)
}


#' Construct the comparison specific Bradley--Terry design matrix
#'
#'
#' @param object1 vector containing the labels of object1
#' @param object2 vector containing the labels of object2
#' @return design matrix X
#'
#' @keywords internal
#'
#' @examples
#'
#'
#' #Generate comparisons of three players
#' object1 <- c(1, 3, 2, 1)
#' object2 <- c(3, 1, 1, 2)
#' X <- construct.design.matrix.by.comparison(object1, object2)
#'
#' @keywords internal
#'
#' @export
construct.design.matrix.by.comparison <- function (object1, object2) {
  K <- length(object1)
  n.objects <- max(c(object1, object2))
  term1 <- Matrix::sparseMatrix(i = 1:K, j = object1, x = 1, dims = c(K, n.objects))
  term2 <- Matrix::sparseMatrix(i = 1:K, j = object2, x = -1, dims = c(K, n.objects))
  X <- term1 + term2
  return(X)
}


#' Construct the Bradley--Terry design matrix for ties
#' This is the design matrix for tied comparisons. Each permutation
#' (rather than combination) is featured, i.e. both (i, j) and (j, i).
#'
#' @param n.objects the number of objects
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
construct.design.matrix.both.ways <- function (n.objects) {
  all.pairs <- t(utils::combn(n.objects, 2))
  a <- c(all.pairs[, 1], all.pairs[, 2])
  b <- c(all.pairs[, 2], all.pairs[, 1])
  all.pairs <- cbind(a, b)
  winners <- Matrix::sparseMatrix(i = 1:(n.objects * (n.objects - 1)),
                                  j = all.pairs[, 1], x = 1, dims = c(n.objects^2 - n.objects, n.objects))
  losers <- Matrix::sparseMatrix(i = 1:(n.objects * (n.objects - 1)),
                                 j = all.pairs[, 2], x = -1, dims = c(n.objects^2 - n.objects, n.objects))
  X <- winners + losers
  return(X)
}

#' Standard Bayesian Bradley--Terry model
#'
#' This function uses MCMC to sample from the posterior distribution of the
#' standard Bradley--Terry model. Standard model means that there are no tied
#' comparisons and no player or comparison specific variables. This provides a
#' fast implementation of the standard model. A multivariate normal prior
#' distribution on the player quality parameters can be specified.
#'
#'
#'
#' @param outcome vector of outcomes. 1 if player 2 is the winner,
#'  0 if player 1 is the winner
#' @param player1 vector of first players
#' @param player2 vector of second players
#' @param win.matrix a win-loss matrix where the i,j th element is the number of
#' times object i beat object j
#' @param player.prior.var (optional) matrix specifying the prior covariance of
#'  the player correlation parameters
#' @param lambda.initial (optional) vector containing the values of the
#'  player correlation parameters for the first MCMC iteration
#' @param n.iter number of MCMC samples to be drawn
#' @param hyperparameter boolean indicating if inference should be performed
#'  for the prior variance hyperparameter. If TRUE the prior variance
#'  (main diagonal of the covariance matrix) must be set to 1.
#' @param psi shape parameter for the inverse-gamma prior distribution on the
#' hyperparameter
#' @param chi rate parameter for the inverse-gamma prior distribution on the
#'  hyperparameter
#'
#' @details If `player.prior.var` is omitted, independent and identical
#' N(0, 1^2) prior distributions are placed on each object quality parameter.
#'
#' If `lambda.initial` is ommitted, it is set to a vector of zeroes.
#'
#'
#' @return  A data frame containing samples from the posterior distribution
#'
#'
#' @examples
#'
#' ########################################
#' ## Forced Marriage in Nottinghamshire ##
#' ########################################
#'
#' #Construct covariance matrix based on spatial information
#' sigma <- expm::expm(forcedMarriage$adjacencyMatrix)
#' sigma <- diag(diag(sigma)^-0.5)%*% sigma %*%diag(diag(sigma)^-0.5)
#'
#'
#' ##Not Run
#' #Fit model
#' #forcedMarriageModel <- speedyBBTm(outcome = rep(1, length(forcedMarriage$comparisons$win)),
#' #                                  player1 = forcedMarriage$comparisons$win,
#' #                                  player2 = forcedMarriage$comparisons$lost,
#' #                                 player.prior.var = sigma)
#'
#' #Plot results
#' #plot(sort(forcedMarriageQualitySamples))
#'
#' @export
#'
speedyBBTm <- function(outcome = NULL, player1 = NULL, player2 = NULL,
                       win.matrix = NULL, player.prior.var = NULL,
                       lambda.initial = NULL, n.iter = 1000, hyperparameter = TRUE,
                       chi = 0.01, psi = 0.01){

  if(is.null(win.matrix)){ #Create win matrix
  #get number of objects in study
  n.objects <- max(c(player1, player2))

  #Get winner and loser of each comparison
  winner <- ifelse(outcome == 1, player2, player1)
  loser  <- ifelse(outcome == 0, player2, player1)


  #Turn each comparison (except ties) into a win/loss matrix
  win.matrix <- comparisons_to_matrix(n.objects, data.frame(winner, loser))
  } else {
    n.objects <- dim(win.matrix)[1]
  }

  #Get y_ij and n_ij (the number of times i beat j and i and
  # j were compared respectively)
  y <- win.matrix[lower.tri(win.matrix)]
  n <- (win.matrix + t(win.matrix))[lower.tri(win.matrix)]
  y <- n - y

  #Construct the design matrix
  X <- construct.design.matrix(n.objects)

  #Get inverse of prior covariance matrix
  #If not set, the prior is iid N(0,1^2)
  if(is.null(player.prior.var))
    player.prior.var <- diag(n.objects)
  player.prior.var.inverse <- solve(player.prior.var)

  #Set initial values for lambda
  if(is.null(lambda.initial))
    lambda.initial <- numeric(n.objects)
  if(n.objects != length(lambda.initial))
  stop("Mismatch between number of objects in study and length of
       vector for initial estimates.")


  #Remove pairs that were never compared
  non.zero.n       <- length(n[n!=0])
  X                <- X[which(n!=0), ]
  y                <- y[which(n!=0)]
  n                <- n[which(n!=0)]

  #Set up initial values
  lambda           <- lambda.initial
  alpha.sq         <- 1

  #Set up empty storage spaces
  lambda.matrix    <- matrix(0, n.iter, n.objects)
  alpha.sq.vector  <- numeric(n.iter)


  #Set commonly required constants
  unnormalised.mu  <- Matrix::t(X)%*%(y - n/2)
  grand.covariance <- sum(player.prior.var)


  #Set iteration counter
  pb <- utils::txtProgressBar(min = 0, max = n.iter, style = 3)

  #MCMC loop
  for(i in 1:n.iter){

    if(hyperparameter == TRUE)
      alpha.sq     <- 1/stats::rgamma(1, chi + n.objects/2,
                                      0.5*t(lambda)%*%player.prior.var.inverse%*%lambda + psi)

    z            <- BayesLogit::rpg(non.zero.n, n, as.numeric(X%*%lambda))
    Z            <- Matrix::sparseMatrix(i = 1:non.zero.n, j = 1:non.zero.n, x = z)
    V            <- base::chol2inv(base::chol(Matrix::t(X)%*%Z%*%X
                                              + player.prior.var.inverse/alpha.sq))
    mu           <- V%*%unnormalised.mu
    V.chol       <- base::chol(V)
    lambda       <- as.numeric(t(V.chol)%*%stats::rnorm(n.objects, 0, 1) + mu)

    #Translate quality parameters
    lambda       <- lambda - mean(lambda)

    lambda.matrix[i, ] <- lambda
    alpha.sq.vector[i] <- alpha.sq
    utils::setTxtProgressBar(pb, i) # update text progress bar after each iter
  }
  if(hyperparameter == TRUE)
    return(list("lambda" = lambda.matrix, "alpha.sq" = alpha.sq.vector))
  else
    return(list("lambda" = lambda.matrix))
}



#' Bayesian inference for the Bradley--Terry model with ties
#'
#' This function uses MCMC to sample from the posterior distribution of the
#' Bradley--Terry model with ties.A multivariate normal prior
#' distribution on the player quality parameters can be specified. An exponential
#' prior distribution is placed on the tie parameter theta, and a Metropolis-
#' Hasting random walk algorithm is used to update this parameter.
#'
#' @param n.objects number of objects in the study
#' @param outcome vector of outcomes. 0 if player 1 is the winner,
#'  1 if player 2 is the winner, and 2 if it is a tie.
#' @param player1 vector of first players.
#' @param player2 vector of second players.
#' @param player.prior.var (optional) matrix specifying the prior covariance of
#'  the player correlation parameters
#' @param lambda.initial (optional) vector containing the values of the
#'  player parameters for the first MCMC iteration
#' @param theta.initial (optional) value of the tied parameter there for
#' the first MCMC iteration
#' @param theta.rate (optional) The rate parameter of the exponential prior
#' distribution placed on theta
#' @param n.iter number of MCMC samples to be drawn
#' @param hyperparameter boolean indicating if inference should be performed
#'  for the prior variance hyperparameter. If TRUE the prior variance
#'  (main diagonal of the covariance matrix) must be set to 1.
#' @param psi shape parameter for the inverse-gamma prior distribution on the
#' hyperparameter
#' @param chi rate parameter for the inverse-gamma prior distribution on the
#'  hyperparameter
#' @param rw.sd number describing the standard deviation of normal distribution
#' proposal distribution for theta
#'
#' @details If `player.prior.var` is omitted, independent and identical
#' N(0, 5^2) prior distributions are placed on each object quality parameter.
#'
#' If `lambda.initial` is omitted, it is set to a vector of zeroes.
#'
#'
#' @return  A data frame containing samples from the posterior distribution
#'
#'
#' @examples
#'
#' ############################################
#' ## Deprivation in Dar es Salaam, Tanzania ##
#' ## Seymour et al (2022)                   ##
#' ############################################
#'
#'#Construct covariance matrix based on spatial informartion
#' sigma <- expm::expm(darEsSalaam$adjacencyMatrix)
#' sigma <- diag(diag(sigma)^-0.5)%*% sigma %*%diag(diag(sigma)^-0.5)
#'
#'##Not Run
#'
#' #Fit BT model with ties
#'#darTiedModel <- BBTm.ties(n.objects = 452,
#'#                          outcome = darEsSalaam$comparisons$outcome,
#'#                          player1 = darEsSalaam$comparisons$subward1,
#'#                          player2 = darEsSalaam$comparisons$subward2,
#'#                          player.prior.var = sigma,
#'#                          hyperparameter = TRUE, rw.sd = 0.005)
#'
#'#Get posterior means
#'#darTiedModel$lambda <- darTiedModel $lambda - colMeans(darTiedModel$lambda)
#'#lambda.mean <- rowMeans(darTiedModel$lambda)
#'
#'#Generate trace plots
#'#plot(lambda.mean)
#'#plot(darTiedModel$theta[-c(1:100)], type = 'l')
#'
#' @export
#'
BBTm.ties <- function(n.objects, outcome, player1, player2, player.prior.var = NULL,
                      theta.initial = NULL, lambda.initial = NULL, n.iter = 1000,
                      hyperparameter = TRUE,
                      chi = 0.01, psi = 0.01, rw.sd = 0.1, theta.rate = 0.01){


  #get number of objects in study
  n.objects <- max(c(player1, player2))

  #Order pairs into winner/loser
  winner <- ifelse(outcome[outcome != 2] == 1, player2[outcome != 2], player1[outcome != 2])
  loser  <- ifelse(outcome[outcome != 2] == 0, player2[outcome != 2], player1[outcome != 2])


  #Turn each comparison (except ties) into a win/loss matrix
  win.matrix <- comparisons_to_matrix(n.objects, data.frame(winner, loser))

  tie.matrix <- matrix(0, n.objects, n.objects)
  for(j in which(outcome == 2)){
    tie.matrix[player1[j], player2[j]] <- tie.matrix[player1[j], player2[j]] + 1
    tie.matrix[player2[j], player1[j]] <- tie.matrix[player2[j], player1[j]] + 1
  }

  X                   <- construct.design.matrix.both.ways(n.objects)

  #Get inverse of prior covariance matrix
  if(is.null(player.prior.var))
    player.prior.var <- 5^2*diag(n.objects)
  player.prior.var.inverse <- solve(player.prior.var)

  #Set initial values for lambda
  if(is.null(lambda.initial))
    lambda.initial <- numeric(n.objects)
  if(n.objects != length(lambda.initial))
    stop("Mismatch between number of objects in study and length of vector for initial estimates.")

  y                   <- c(t(win.matrix)[lower.tri(win.matrix)],
                           win.matrix[lower.tri(win.matrix)])
  t                   <- c(t(tie.matrix)[lower.tri(tie.matrix)],
                           tie.matrix[lower.tri(tie.matrix)])


  #Remove pairs of players that were never compared
  kappa           <- (y + t)/2
  non.zero.kappa <- length(kappa[kappa != 0])
  X          <- X[which(kappa != 0), ]
  t          <- t[which(kappa != 0)]
  y          <- y[which(kappa != 0)]


  if(is.null(theta.initial))
    theta.initial <- 0.5

  #Set constants for MCMC
  lambda     <- lambda.initial
  theta      <- theta.initial
  ones       <- rep(1, non.zero.kappa)
  kappa      <- (y + t)/2
  alpha.sq           <- 1
  grand.covariance   <- sum(player.prior.var)

  #Create empty storage vessels
  lambda.matrix      <- matrix(0, n.objects, n.iter)  #store results
  theta.store        <- numeric(n.iter)  #store results
  alpha.sq.store     <- numeric(n.iter)  #store results

  pb                 <- utils::txtProgressBar(min = 0, max = n.iter, style = 3)

  #MCMC
  for(i in 1:n.iter){

    #Update alpha^2
    if(hyperparameter == TRUE)
      alpha.sq     <- 1/stats::rgamma(1, 0.01 + n.objects/2, 0.5*t(lambda)%*%player.prior.var.inverse%*%lambda + 0.01)

    #Update Z
    z              <- BayesLogit::rpg(non.zero.kappa, y + t, as.numeric(X%*%lambda) - theta)
    Z              <- Matrix::sparseMatrix(i = 1:non.zero.kappa, j = 1:non.zero.kappa, x = z)

    #Update lambda
    V              <- chol2inv(chol(Matrix::t(X)%*%Z%*%X + player.prior.var.inverse/alpha.sq))
    mu             <- V%*%(Matrix::t(X)%*%(kappa + theta*Z%*%ones))
    V.chol         <- chol(V)
    lambda         <- as.numeric(t(V.chol)%*%stats::rnorm(n.objects, 0, 1) + mu)
    lambda         <- lambda - mean(lambda) #translate to have mean 0



    #Update theta
    theta.prop   <- theta + stats::rnorm (1, 0, rw.sd)
    if(theta.prop > 0){
      loglike      <- sum(t)/2*log(exp(2*theta) - 1)  - sum((y+t)*(theta + log(1 + exp((X%*%lambda) - theta))))
      loglike.prop <- sum(t)/2*log(exp(2*theta.prop) - 1) - sum((y+t)*(theta.prop + log(1 + exp((X%*%lambda) - theta.prop))))
      log.p.acc    <- loglike.prop - loglike + stats::dexp(theta.prop, theta.rate, log = TRUE) - stats::dexp(theta, theta.rate, log = TRUE)
      if(log(stats::runif(1)) < log.p.acc)
        theta      <- theta.prop
    }


    theta.store[i]     <- theta
    alpha.sq.store[i]  <- alpha.sq
    lambda.matrix[, i] <- as.numeric(lambda)
    utils::setTxtProgressBar(pb, i) # update text progress bar after each iter
  }

  if(hyperparameter == TRUE)
    return(list("lambda" = lambda.matrix, "theta" = theta.store, "alpha.sq" = alpha.sq.store))
  else
    return(list("lambda" = lambda.matrix, "theta" = theta.store))

}




#' Bayesian Bradley--Terry model with comparison specific effect
#'
#'This function fits the Bradley-Terry model with a comparison specific effect.
#'Each comparison can be assigned a real value to allow for a specific effect
#'for the comparison, such as bias, ordering or home/away effect. The value of
#'this effect is denoted $kappa$. The function places a normal prior distribution
#'on both kappa and the player quality parameters lambda.
#'
#'
#'
#' @param outcome vector of outcomes. 1 if player2 is the winner,
#'  0 if player1 is the winner
#' @param player1 vector of first players.
#' @param player2 vector of second players.
#' @param player.prior.var (optional) matrix specifying the prior covariance of
#'  the player correlation parameters
#' @param lambda.initial (optional) vector containing the values of the
#'  player parameters for the first MCMC iteration
#' @param n.iter number of MCMC samples to be drawn
#' @param advantage (optional) a vector with the value of the comparisons specific
#'  effect for each comparison
#' @param kappa.initial (optional) an initial value for the comparison specific
#'  value kappa
#'  @param kappa.var (optional) the prior variance of the he comparison specific
#'  value kappa
#' @param hyperparameter boolean indicating if inference should be performed
#'  for the prior variance hyperparameter. If TRUE the prior variance
#'  (main diagonal of the covariance matrix) must be set to 1.
#' @param psi shape parameter for the inverse-gamma prior distribution on the
#' hyperparameter
#' @param chi rate parameter for the inverse-gamma prior distribution on the
#'  hyperparameter
#'
#' @details If `player.prior.var` is omitted, independent and identical
#' N(0, 5^2) prior distributions are placed on each object quality parameter.
#'
#' If `lambda.initial` is omitted, it is set to a vector of zeroes.
#'
#' If `lambda.var` is omitted, it is set to N(0, 5^2).
#'
#'
#' @return  A data frame containing samples from the posterior distribution
#'
#' @keywords internal
#'
#' @export
#'
BBTm.no.formula <- function(outcome, player1, player2, player.prior.var,
                       lambda.initial, advantage = NULL, kappa.initial = NULL,
                       kappa.var = NULL, n.iter = 1000, hyperparameter = TRUE,
                       chi = 0.01, psi = 0.01){

  #get number of objects in study
  n.objects <- max(c(player1, player2))

  #get number of comparisons
  n.comp <- length(outcome)

  #Get winner and loser of each comparison
  winner <- ifelse(outcome == 1, player2, player1)
  loser  <- ifelse(outcome == 0, player2, player1)

  #Get y_ij
  y <- 1- outcome
  k <- y - 0.5

  #Construct the design matrix
  X  <- construct.design.matrix.by.comparison(player1, player2)


  #Get inverse of prior covariance matrix
  #If not set, the prior is iid N(0, 5^2)
  if(is.null(player.prior.var))
    player.prior.var <- 5^2*diag(n.objects)
  player.prior.var.inverse <- solve(player.prior.var)

  #Set initial values for lambda
  if(is.null(lambda.initial))
    lambda.initial <- numeric(n.objects)
  if(n.objects != length(lambda.initial))
    stop("Mismatch between number of objects in study and length of
       vector for initial estimates.")

  #Set up initial values
  lambda           <- lambda.initial
  alpha.sq         <- 1

  #Determine if inference is required for advantages
  if(is.null(advantage)){
    kappa <- 0
    advantage <- rep(0, n.comp)
    advantage.inf <- FALSE
  } else {
    if(is.null(kappa.initial)){
      kappa <- 0.5
    } else{
      kappa <- kappa.initial
    }
    kappa.precision <- 1/kappa.var
    kappa.vector     <- numeric(n.iter)
    advantage.inf   <- TRUE
  }


  #Set up empty storage spaces
  lambda.matrix    <- matrix(0, n.iter, n.objects)
  alpha.sq.vector  <- numeric(n.iter)



  #Set commonly required constants
  grand.covariance <- sum(player.prior.var)


  #Set iteration counter
  pb <- utils::txtProgressBar(min = 0, max = n.iter, style = 3)

  #MCMC loop
  for(i in 1:n.iter){

    if(hyperparameter == TRUE)
      alpha.sq     <- 1/stats::rgamma(1, chi + n.objects/2,
                                      0.5*t(lambda)%*%player.prior.var.inverse%*%lambda + psi)

    #Update Z
    z              <- BayesLogit::rpg(n.comp, 1, as.numeric(X%*%lambda) +
                                        kappa*advantage)
    Z              <- Matrix::sparseMatrix(i = 1:n.comp, j = 1:n.comp, x = z)

    #Update lambda
    V              <- chol2inv(chol(Matrix::t(X)%*%Z%*%X + player.prior.var.inverse))
    mu             <- V%*%(Matrix::t(X)%*%(k - kappa*Z%*%advantage))
    V.chol         <- chol(V)
    lambda         <- as.numeric(t(V.chol)%*%stats::rnorm(n.objects, 0, 1) + mu)

    #Update kappa
    if(advantage.inf){
      S             <- (t(advantage)%*%Z%*%advantage + kappa.precision)^-1
      mu.kappa      <- S*t(advantage)%*%(k - Z%*%X%*%lambda)
      kappa         <- stats::rnorm(1, as.numeric(mu.kappa), as.numeric(S))
      kappa.vector[i] <- kappa
    }



    #Translate quality parameters
    lambda       <- lambda - mean(lambda)

    lambda.matrix[i, ] <- lambda
    alpha.sq.vector[i] <- alpha.sq



    utils::setTxtProgressBar(pb, i) # update text progress bar after each iter
  }
  if(hyperparameter == TRUE & advantage.inf == TRUE){ #Output alpha.sq and kappa
    return(list("lambda" = lambda.matrix, "alpha.sq" = alpha.sq.vector,
                "kappa" = kappa.vector))
  } else if(hyperparameter == FALSE & advantage.inf == TRUE){ #Output only kappa
    return(list("lambda" = lambda.matrix, "kappa" = kappa.vector))
  } else if(hyperparameter == FALSE & advantage.inf == FALSE){ #Output only alpha.sq
    return(list("lambda" = lambda.matrix, "alpha.sq" = alpha.sq.vector))
  } else
    return(list("lambda" = lambda.matrix))
}





#' Bayesian Bradley--Terry model with comparison and player specific effect and formula
#'
#'This function fits the Bradley-Terry model with comparison  and player
#'specific effects. Each comparison can be assigned a real value to allow for a
#'specific effect for the comparison, such as bias, ordering or home/away effect.
#'The value of this effect is denoted kappa. The player specific effects are
#'described through a formula and data.frame containing the value. The function
#'places a normal prior distribution on both kappa and the player specific
#'parameters beta.
#'
#'
#'
#' @param outcome vector of outcomes. 1 if player2 is the winner,
#'  0 if player1 is the winner
#' @param player1 vector of first players.
#' @param player2 vector of second players.
#' @param formula formula with no left-hand-side specifying the player specific
#' effects
#' @param data data.frame with a row corresponding to each player and  column corresponding
#' to each covariate.
#' @param player.prior.var (optional) matrix specifying the prior covariance of
#'  the player correlation parameters
#' @param beta.initial (optional) vector containing the values of the
#'  player specific  parameters for the first MCMC iteration
#' @param n.iter number of MCMC samples to be drawn
#' @param advantage (optional) a vector with the value of the comparisons specific
#'  effect for each comparison
#' @param kappa.initial (optional) an initial value for the comparison specific
#'  value kappa
#' @param kappa.var (optional) the prior variance of the he comparison specific
#'  value kappa
#' @param hyperparameter boolean indicating if inference should be performed
#'  for the prior variance hyperparameter. If TRUE the prior variance
#'  (main diagonal of the covariance matrix) must be set to 1.
#' @param psi shape parameter for the inverse-gamma prior distribution on the
#' hyperparameter
#' @param chi rate parameter for the inverse-gamma prior distribution on the
#'  hyperparameter
#'
#' @details If `player.prior.var` is omitted, independent and identical
#' N(0, 5^2) prior distributions are placed on each object quality parameter.
#'
#' If `beta.initial`is omitted, it is set to a vector of zeroes.
#'
#' If `kappa.var` is omitted, it is set to N(0, 5^2), if `kappa.initial` is omitted
#' it is set to 0.5.
#'
#'
#' @return  A data frame containing samples from the posterior distribution
#'
#' @keywords internal
#'
#' @export
#'

BBTm.with.formula <- function(outcome, player1, player2,
                              formula = NULL, data = NULL,
                              advantage = NULL, kappa.initial = NULL, kappa.var = NULL,
                              player.prior.var = NULL, beta.initial = NULL, n.iter = 1000,
                              hyperparameter = TRUE, chi = 0.01, psi = 0.01){

  #get number of objects in study
  n.objects <- max(c(player1, player2))
  n.comp <- length(outcome)


  #Get y_ij
  y <- 1 - outcome
  k <- y - 0.5

  #Construct the design matrix
  X <- construct.generalised.design.matrix(player1, player2, formula, data)
  formula.model <- stats::model.frame(formula, data)



  #Get inverse of prior covariance matrix
  if(is.null(player.prior.var) & hyperparameter == FALSE){
    player.prior.var <- 5^2*diag(dim(X)[2]) #fix prior to be N( 0, 5^2)
  } else if(is.null(player.prior.var)){
    player.prior.var <- diag(dim(X)[2])
  }
  player.prior.var.inverse <- solve(player.prior.var)

  #Determine if inference is required for advantages
  if(is.null(advantage)){
    kappa <- 0
    advantage <- rep(0, n.comp)
    advantage.inf <- FALSE
  } else {
    if(is.null(kappa.initial)){
      kappa <- 0.5
    } else{
      kappa <- kappa.initial
    }

    if(is.null(kappa.var)){
      kappa.var <- 5^2
    }

    kappa.precision <- 1/kappa.var
    kappa.vector     <- numeric(n.iter)
    advantage.inf   <- TRUE
  }


  #Set initial values for beta
  if(is.null(beta.initial))
    beta.initial <- numeric(dim(X)[2])
  if(dim(X)[2] != length(beta.initial))
    stop("Mismatch between number of covariates in study and length of vector for initial estimates.")


  #Set up MCMC
  beta             <- beta.initial
  alpha.sq         <- 1

  beta.matrix      <- matrix(0, n.iter, dim(X)[2])
  lambda.matrix    <- matrix(0, n.iter, n.objects)
  alpha.sq.vector  <- numeric(n.iter)
  grand.covariance <- sum(player.prior.var)



  pb <- utils::txtProgressBar(min = 0, max = n.iter, style = 3)
  for(i in 1:n.iter){

    if(hyperparameter == TRUE)
      alpha.sq     <- 1/stats::rgamma(1, chi + dim(X)[2]/2, 0.5*t(beta)%*%player.prior.var.inverse%*%beta + psi)

    z       <- BayesLogit::rpg(n.comp, 1, as.numeric(X%*%beta) +
                                      kappa*advantage)
    Z      <- Matrix::sparseMatrix(i = 1:n.comp, j = 1:n.comp, x = z)
    V            <- base::chol2inv(base::chol(Matrix::t(X)%*%Z%*%X + player.prior.var.inverse/alpha.sq))
    mu           <- V%*%(Matrix::t(X)%*%(k - kappa*Z%*%advantage))
    V.chol       <- base::chol(V)
    beta         <- as.numeric(t(V.chol)%*%stats::rnorm(dim(X)[2], 0, 1) + mu)

    #Update kappa
    if(advantage.inf){
      S             <- (t(advantage)%*%Z%*%advantage + kappa.precision)^-1
      mu.kappa      <- S*t(advantage)%*%(k - Z%*%X%*%beta)
      kappa         <- stats::rnorm(1, as.numeric(mu.kappa), as.numeric(S))
      kappa.vector[i] <- kappa
    }

    #Translate quality parameters
    lambda       <- as.matrix(formula.model)%*%beta
    lambda       <- lambda - mean(lambda)

    beta.matrix[i, ] <- t(beta)
    lambda.matrix[i, ] <- lambda
    alpha.sq.vector[i] <- alpha.sq
    utils::setTxtProgressBar(pb, i) # update text progress bar after each iter
  }
  if(hyperparameter == TRUE & advantage.inf == TRUE){ #Output alpha.sq and kappa
    return(list("beta" = beta.matrix, "lambda" = lambda.matrix, "alpha.sq" = alpha.sq.vector,
                "lambda" = lambda.matrix, "kappa" = kappa.vector))
  } else if(hyperparameter == FALSE & advantage.inf == TRUE){ #Output only kappa
    return(list("beta" = beta.matrix,
                 "lambda" = lambda.matrix, "kappa" = kappa.vector))
  } else if(hyperparameter == FALSE & advantage.inf == FALSE){ #Output only alpha.sq
    return(list("beta" = beta.matrix, "lambda" = lambda.matrix, "alpha.sq" = alpha.sq.vector))
  } else
    return(list("beta" = beta.matrix, "lambda" = lambda.matrix))
}




#' Generalised Bradley-Terry model
#'
#'This function fits the Bradley-Terry model with comparison  and player
#'specific effects. Each comparison can be assigned a real value to allow for a
#'specific effect for the comparison, such as bias, ordering or home/away effect.
#'The value of this effect is denoted kappa. The player specific effects are
#'described through a formula and data.frame containing the value. The function
#'places a normal prior distribution on both kappa and the player specific
#'parameters beta.
#'
#'
#' @inheritParams BBTm.with.formula
#'
#' @param lambda.initial (optional) vector containing the values of the
#'  player parameters for the first MCMC iteration
#'
#'
#'
#' @details If `player.prior.var` is omitted, independent and identical
#' N(0, 5^2) prior distributions are placed on each object quality parameter.
#'
#' If `beta.initial`is omitted, it is set to a vector of zeroes.
#'
#' If `kappa.var` is omitted, it is set to N(0, 5^2), if `kappa.initial` is omitted
#' it is set to 0.5.
#'
#'
#' @return  A data frame containing samples from the posterior distribution
#'
#'
#'
#' @examples
#'
#' #####################
#' ## Wimbledon 2019 ##
#' ####################
#'
#'#Fit model where the quality of each player depends on their rank
#'#and the number of points they had immediately before the tournament.
#'#Allow an effect for a match being in the first or second week.
#'#wimbledonModel <- BBTm(outcome  = wimbledon$matches$outcome,
#'#                      player2   = wimbledon$matches$loser,
#'#                       player1  = wimbledon$matches$winner,
#'#                      advantage = wimbledon$matches$secondWeek,
#'#                      formula  = ~ rank + points,
#'#                      data       = wimbledon$players,
#'#                       n.iter = 4000)
#'
#' #Plot posterior distributions
#'  #hist(wimbledonModel$kappa[-c(1:100)], main = "", xlab = expression(kappa), freq  = FALSE)
#'  #hist(wimbledonModel$beta[-c(1:100), 1], main = "", xlab = expression(beta[1]), freq  = FALSE)
#'  #hist(wimbledonModel$beta[-c(1:100), 2], main = "", xlab = expression(beta[2]), freq  = FALSE)
#'
#'
#' @export
#'
BBTm <- function(outcome, player1, player2, lambda.initial = NULL,
                 player.prior.var = NULL , beta.initial = NULL, n.iter = 1000,
                 formula = NULL, data = NULL,
                 advantage = NULL , kappa.initial = NULL, kappa.var = NULL,
                 hyperparameter = TRUE, chi = 0.01, psi = 0.01){

  if(!is.null(lambda.initial) & !is.null(beta.initial))
    stop("Cannot set initial values for both lambda and beta")

  if(!is.null(formula)){
  output <- BBTm.with.formula(outcome, player1, player2,
                              formula, data,
                              advantage, kappa.initial, kappa.var,
                              player.prior.var, beta.initial, n.iter,
                              hyperparameter, chi, psi)
  } else{
  output <- BBTm.no.formula(outcome, player1, player2, player.prior.var,
                     lambda.initial,
                     advantage, kappa.initial, kappa.var,
                     n.iter, hyperparameter, chi, psi)

  }
  return(output)

}



