#' Plot Quality Parameter Posteriors
#'
#' Generates a plot of the posterior mean and 95% credible interval
#' of the quality parameter estimates.
#'
#' @param model_output The ["mcmc"][coda::mcmc] model output object containing the draws.
#' @param player_names A character vector containing the names of the items (e.g., players or wards).
#' @param flip Whether to flip the plot to put the quality estimates on the x-axis instead of the y-axis;
#' defaults to FALSE.
#' @param main The text to use for the plot title.
#' @param xlab The text to use for the x-axis label.
#' @param ylab The text to use for the y-axis label.
#' @param quality_label A vector of three custom labels to use instead of numbers on the quality axis.
#' @param ... other arguments passed to `plot`
#' @export
#' @importFrom graphics axis mtext par segments
#' @importFrom stats quantile median
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
#' plot_qualities(model_output = wimbledonModel, player_names = players)
plot_qualities <- function(
  model_output,
  player_names,
  flip = FALSE,
  main = "Posterior Mean and 95% Credible \n Interval of Player Quality",
  xlab = "Player",
  ylab = "Posterior Mean Quality",
  quality_label = NULL,
  ...
) {
  # Posterior mean and 95% credible intervals (burn-in = 100)

  param_draws <- parameter(model_output, "lambda")
  model_means <- colMeans(param_draws)
  if (length(player_names) != length(model_means)) {
    stop(
      "The length of player_names must match the expected number of quality estimates."
    )
  }
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
  results <- data.frame(
    "item" = player_names,
    "mean" = model_means,
    "lowerCI" = modelLowerCI,
    "upperCI" = modelUpperCI
  )
  results <- results[
    order(results$mean),
  ]
  n.items <- nrow(results)

  if (flip) {
    mar_orig <- c(4, 11, 4, 2)
  } else {
    mar_orig <- c(11, 4, 4, 2)
  }
  oldpar <-
    par(
      mfrow = c(1, 1),
      mar = mar_orig, # large bottom margin for vertical labels
      mgp = c(10, 1, 0) # axis title, tick labels, tick marks distance from axis
    )
  on.exit(par(oldpar), add = TRUE)

  if (flip) {
    plot(
      x = results$mean,
      y = 1:n.items,
      yaxt = "n",
      xaxt = "n",
      xlab = "",
      xlim = c(min(modelLowerCI), max(modelUpperCI)),
      ylab = "",
      main = main,
      ...
    )
    segments(
      y0 = 1:n.items,
      x0 = results$lowerCI,
      x1 = results$upperCI
    )
    player_axis <- 2
    quality_axis <- 1
  } else {
    plot(
      x = results$mean,
      xaxt = "n",
      yaxt = "n",
      ylab = "",
      ylim = c(min(modelLowerCI), max(modelUpperCI)),
      xlab = "",
      main = main,
      ...
    )
    segments(
      x0 = 1:n.items,
      y0 = results$lowerCI,
      y1 = results$upperCI
    )
    player_axis <- 1
    quality_axis <- 2
  }

  axis(
    player_axis,
    at = 1:n.items,
    labels = player_names,
    las = 2
  )

  if (is.null(quality_label)) {
    axis(quality_axis)
  } else {
    axis(
      quality_axis,
      labels = quality_label,
      las = 1,
      at = c(
        min(results$lowerCI),
        median(results$mean),
        0.95 * max(results$upperCI)
      )
    )
  }
  mtext(xlab, side = player_axis, line = 10)
  mtext(ylab, side = quality_axis, line = 3)

  return(results)
}
