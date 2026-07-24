#' Plot quality parameter posteriors
#'
#' Generates a plot of the posterior mean and 95% credible interval
#' of the quality parameter estimate.
#'
#' @param item_names The names of the different items (or players or wards) being compared.
#' @param model_output The model output object containing the draws.
#'
#' @examples
#'
#' items <- wimbledon$players$name
#'
#' wimbledonModel <- BBTm(
#'   outcome = wimbledon$matches$outcome,
#'   player2 = wimbledon$matches$loser,
#'   player1 = wimbledon$matches$winner,
#'   advantage = wimbledon$matches$secondWeek,
#'   formula = ~ rank + points,
#'   data = wimbledon$players,
#'   n.iter = 200
#' )
#'
#' plot_qualities(item_names = items, model_output = wimbledonModel)
#'
plot_qualities <- function(item_names, model_output) {
  # Posterior mean and 95% credible intervals (burn-in = 100)
  param_draws <- parameter(model_output, "lambda")
  model_means <- colMeans(param_draws)
  modelLowerCI <- apply(
    param_draws,
    2,
    quantile,
    0.025
  )
  modelUpperCI <- apply(
    param_draws,
    2,
    quantile,
    0.975
  )
  modelResults <- data.frame(
    "item" = item_names,
    "mean" = model_means,
    "lowerCI" = modelLowerCI,
    "upperCI" = modelUpperCI
  )
  modelResults <- modelResults[
    order(modelResults$mean),
  ]

  par(mfrow = c(1, 1))
  plot(
    modelResults$mean,
    xaxt = "n",
    ylab = "Posterior Mean Quality",
    ylim = c(-7.5, 7.5)
  )
  segments(
    x0 = 1:nrow(modelResults),
    y0 = modelResults$lowerCI,
    y1 = modelResults$upperCI
  )
  axis(
    1,
    at = 1:nrow(modelResults),
    labels = item_names,
    las = 2
  )
}
