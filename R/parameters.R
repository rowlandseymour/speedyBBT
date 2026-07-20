#' This function helps the user extract relevant estimates from the quality parameters
#' fit using a Bayesian Bradley-Terry model.
#'
#' @param model_output The mcmc object that contains the draws output from the BBT model
#' @param indices_to_extract a single column or a vector of column indices to extract from the draws matrix
#' @return a matrix with the selected indices, or the full matrix of draws
lambda <- function(model_output, indices_to_extract = NULL) {
  check_model(model_output, "lambda")

  if (is.null(indices_to_extract)) {
    var_string <- grep("lambda", varnames(model_output), value = TRUE)
  } else {
    var_string <- paste(
      "lambda[",
      indices_to_extract,
      "]",
      sep = ""
    )
  }
  lambda_obj <- coda::as.mcmc(
    model_output[, var_string],
    start = mcpar[1],
    end = mcpar[2],
    thin = mcpar[3]
  )
  coda::varnames(lambda_obj) <- var_string
  attr(lambda_obj, 'mcpar') <- coda::mcpar(model_output)
  return(lambda_obj)
}


#' This function helps the user extract relevant estimates from the variance parameter
#' fit using a Bayesian Bradley-Terry model.
#'
#' @param model_output The mcmc object that contains the draws output from the BBT model
#' @return the draws of the parameter
#' @export
parameter <- function(model_output, parameter_name) {
  check_model(model_output, parameter_name)

  return(model_output[, grep(parameter_name, varnames(model_output))])
}

#' internal helper
#' @importFrom rlang abort
check_model <- function(model_output, parameter_name) {
  if (class(model_output) != "mcmc") {
    rlang::abort(
      "model_output must be an object of class mcmc for this function to work correctly."
    )
  }

  if (length(grep(parameter_name, varnames(model_output))) == 0) {
    rlang::abort(
      paste(
        "This model object does not contain estimates of the ",
        parameter_name,
        " parameter. Please run a model using the `speedyBBTm` function."
      ),
      sep = ""
    )
  }
}
