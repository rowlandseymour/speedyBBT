#' Plot quality parameter posteriors
#'
#' Generates a plot of the posterior mean and 95% credible interval
#' of the quality parameter estimates.
#'
#' @param player_names A character vector containing the names of the different items (or players or wards) being compared.
#' @param model_output The ["mcmc"][coda::mcmc] model output object containing the draws.
#' @export
#' @examples
#'
#' players <- wimbledon$players$name
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
#' plot_qualities(player_names = players, model_output = wimbledonModel)
plot_qualities <- function(player_names, model_output) {
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
    "item" = player_names,
    "mean" = model_means,
    "lowerCI" = modelLowerCI,
    "upperCI" = modelUpperCI
  )
  modelResults <- modelResults[
    order(modelResults$mean),
  ]
  oldpar <-
    par(
      mfrow = c(1, 1),
      mar = c(12, 4, 4, 2), # large bottom margin for vertical labels
      mgp = c(10, 0.5, 0) # axis title, tick labels, tick marks distance from axis
    )

  plot(
    modelResults$mean,
    xaxt = "n",
    ylab = "",
    ylim = c(min(modelLowerCI), max(modelUpperCI)),
    xlab = "",
    main = "Posterior Mean and 95% Credible Interval of Player Quality",
  )
  segments(
    x0 = 1:nrow(modelResults),
    y0 = modelResults$lowerCI,
    y1 = modelResults$upperCI
  )
  axis(
    1,
    at = 1:nrow(modelResults),
    labels = player_names,
    las = 2
  )
  mtext("Player", side = 1, line = 10)
  mtext("Posterior Mean Quality", side = 2, line = 2.5)
  on.exit(par(oldpar), add = TRUE)
}
