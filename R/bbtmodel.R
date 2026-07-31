#' Constructor for bbtmodel class
#'
#' This function makes a new object of type bbtmodel
#' @param data The object to turn into a bbtmodel object.
#' @param start An integer representing the starting iteration in the model.
#' @param end An integer representing the last iteration in the model.
#' @param thin An integer representing the thinning interval for the model.
#' @param model_type A character string representing the type of model. One of "speedy", "ties", or "bbtm".
#' @param formula An optional formula object representing the model formula.
#' @return An object of class bbtmodel, which is a subclass of coda::mcmc.
new_bbtmodel <- function(
  data,
  start = 1,
  end = numeric(0),
  thin = 1,
  model_type = "bbt",
  formula = NULL,
  varnames = NULL
) {
  # Create a coda::mcmc object
  bbtmodel_obj <- coda::mcmc(data = data, start = start, end = end, thin = thin)

  # Validate model_type and varnames
  model_type <- validate_model_type(model_type, bbtmodel_obj)
  varnames <- validate_varnames(varnames, ncol(data))

  coda::varnames(bbtmodel_obj) <- varnames
  # Check that model_type is one of the allowed values
  # Attach custom attributes for model_type and formula
  attr(bbtmodel_obj, "model_type") <- model_type
  attr(bbtmodel_obj, "formula") <- formula
  attr(bbtmodel_obj, "varnames") <- varnames

  # Inherit coda and add bbtmodel class to class vector
  class(bbtmodel_obj) <- c("bbtmodel", "mcmc")

  return(bbtmodel_obj)
}

#' Validate variable names for bbtmodel
#'
#' This function checks if the provided variable names are valid for a bbtmodel object. If varnames is NULL, it generates default names based on the number of objects. If varnames is provided, it checks that its length matches the number of objects.
#' @param varnames A character vector of variable names or NULL.
#' @param n.objects An integer representing the number of objects in the model.
#' @return A character vector of validated variable names.
validate_varnames <- function(varnames, n.objects) {
  if (is.null(varnames)) {
    return(paste0("lambda[", 1:n.objects, "]"))
  } else {
    if (length(varnames) != n.objects) {
      stop("Length of varnames must match the number of objects.")
    }
    return(varnames)
  }
}

#' Validate model type for bbtmodel
#'
#' This function checks if the provided model type is one of the allowed values: "speedy", "ties", or "bbt". If not, it throws an error.
#' @param model_type A character string representing the type of model.
#' @param bbtmodel_obj An object of class bbtmodel to validate against.
#' @return A character string of the validated model type.
validate_model_type <- function(model_type, bbtmodel_obj) {
  model_type <- match.arg(
    model_type,
    choices = c("speedy", "ties", "bbtm")
  )

  # Check that ties model has additional parameters
  if (model_type == "ties") {
    if (length(grep("theta", varnames(bbtmodel_obj))) == 0) {
      stop(
        "The 'ties' model type requires a theta parameter. Please ensure that the bbtmodel object has the correct variable names."
      )
    }
  }

  return(model_type)
}
