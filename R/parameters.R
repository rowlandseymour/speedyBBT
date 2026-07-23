#' Extract quality parameter draws from a model object
#'
#' This function helps the user extract relevant estimates from the quality parameters
#' fit using a Bayesian Bradley-Terry model.
#'
#' @param model_output The mcmc object that contains the draws output from the BBT model
#' @param indices_to_extract a single column or a vector of column indices to extract from the draws matrix
#' @param parameter_name The name of the parameter matrix to extract
#' @return a matrix with the selected indices, or the full matrix of draws
parameter.matrix <- function(
  model_output,
  parameter_name,
  indices_to_extract = NULL
) {
  check_model(model_output, parameter_name)
#' @param model_output The mcmc object that contains the draws output from the BBT model.
#' @param indices_to_extract A single column or a vector of column indices to extract from the draws matrix.
#' @return A matrix with the selected indices, or the full matrix of draws.
lambda <- function(model_output, indices_to_extract = NULL) {
  check_model(model_output, "lambda")

  if (is.null(indices_to_extract)) {
    var_string <- grep(parameter_name, varnames(model_output), value = TRUE)
  } else {
    var_string <- paste0(
      "lambda[",
      indices_to_extract,
      "]"
    )
  }
  par_obj <- coda::as.mcmc(
    model_output[, var_string],
    start = coda::mcpar(model_output)[1],
    end = coda::mcpar(model_output)[2],
    thin = coda::mcpar(model_output)[3]
  )
  coda::varnames(par_obj) <- var_string
  attr(par_obj, "mcpar") <- coda::mcpar(model_output)
  return(par_obj)
}


#' Extract parameter draws from a model object
#'
#' This function helps the user extract relevant estimates from parameters
#' fit using a Bayesian Bradley-Terry model.
#'
#' @param model_output The mcmc object that contains the draws output from the BBT model.
#' @param parameter_name The name of the parameter to extract draws for.
#' @return A vector containing the draws of the parameter.
#' @export
parameter <- function(model_output, parameter_name) {
  check_model(model_output, parameter_name)

  return(model_output[, grep(parameter_name, varnames(model_output))])
}

#' internal helper
#' @noRd
check_model <- function(model_output, parameter_name) {
  if (!inherits(model_output, "mcmc")) {
    stop(
      "model_output must be an object of class mcmc for this function to work correctly."
    )
  }

  if (length(grep(parameter_name, varnames(model_output))) == 0) {
    stop(
      "This model object does not contain estimates of the ",
      parameter_name,
      " parameter. Please run a model using the `speedyBBTm` function."
    )
  }
}
