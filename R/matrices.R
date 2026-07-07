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
comparisons_to_matrix <- function(n.objects, comparisons) {
  win.matrix <- matrix(0, n.objects, n.objects) #construct empty matrix

  for (j in 1:dim(comparisons)[1]) {
    #for each comparisons, enter outcome into win matrix
    win.matrix[comparisons[j, 2], comparisons[j, 1]] <- win.matrix[
      comparisons[j, 2],
      comparisons[j, 1]
    ] +
      1
  }

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
construct.design.matrix <- function(n.objects) {
  all.pairs <- t(utils::combn(n.objects, 2))
  winners <- Matrix::sparseMatrix(
    i = 1:(n.objects * (n.objects - 1) / 2),
    j = all.pairs[, 1],
    x = 1,
    dims = c(n.objects * (n.objects - 1) / 2, n.objects)
  )
  losers <- Matrix::sparseMatrix(
    i = 1:(n.objects * (n.objects - 1) / 2),
    j = all.pairs[, 2],
    x = -1
  )
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
construct.generalised.design.matrix <- function(
  player1,
  player2,
  formula,
  data
) {
  #Get data frame from formula
  formula.model <- stats::model.frame(formula, data)

  n.objects <- dim(data)[1]
  K <- length(player1)
  all.pairs <- t(utils::combn(n.objects, 2))

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
construct.design.matrix.by.comparison <- function(object1, object2) {
  K <- length(object1)
  n.objects <- max(c(object1, object2))
  term1 <- Matrix::sparseMatrix(
    i = 1:K,
    j = object1,
    x = 1,
    dims = c(K, n.objects)
  )
  term2 <- Matrix::sparseMatrix(
    i = 1:K,
    j = object2,
    x = -1,
    dims = c(K, n.objects)
  )
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
construct.design.matrix.both.ways <- function(n.objects) {
  all.pairs <- t(utils::combn(n.objects, 2))
  a <- c(all.pairs[, 1], all.pairs[, 2])
  b <- c(all.pairs[, 2], all.pairs[, 1])
  all.pairs <- cbind(a, b)
  winners <- Matrix::sparseMatrix(
    i = 1:(n.objects * (n.objects - 1)),
    j = all.pairs[, 1],
    x = 1,
    dims = c(n.objects^2 - n.objects, n.objects)
  )
  losers <- Matrix::sparseMatrix(
    i = 1:(n.objects * (n.objects - 1)),
    j = all.pairs[, 2],
    x = -1,
    dims = c(n.objects^2 - n.objects, n.objects)
  )
  X <- winners + losers
  return(X)
}
