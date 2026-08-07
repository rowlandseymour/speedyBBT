# Standard Bayesian Bradley-Terry model

This function uses MCMC to sample from the posterior distribution of the
standard Bradley–Terry model. Standard model means that there are no
tied comparisons and no item or comparison specific variables. This
provides a fast implementation of the standard model. A multivariate
normal prior distribution on the item quality parameters can be
specified.

## Usage

``` r
speedyBBTm(
  outcome = NULL,
  item1 = NULL,
  item2 = NULL,
  win.matrix = NULL,
  item.prior.var = NULL,
  lambda.initial = NULL,
  n.iter = 1000,
  hyperparameter = TRUE,
  chi = 0.01,
  psi = 0.01,
  burn.in = 100,
  n.thin = 1,
  verbose = interactive()
)
```

## Arguments

- outcome:

  Vector of outcomes. 1 if item 2 is the winner, 0 if item 1 is the
  winner.

- item1:

  Vector of first items.

- item2:

  Vector of second items.

- win.matrix:

  (optional) A win-loss matrix where the i,j th element is the number of
  times object i beat object j.

- item.prior.var:

  (optional) A matrix specifying the prior covariance of the item
  correlation parameters.

- lambda.initial:

  (optional) A vector containing the values of the item correlation
  parameters for the first MCMC iteration.

- n.iter:

  The number of MCMC samples to be drawn.

- hyperparameter:

  A boolean indicating if inference should be performed for the prior
  variance hyperparameter. If TRUE the prior variance (main diagonal of
  the covariance matrix) must be set to 1.

- chi:

  (Optional) The rate parameter for the inverse-gamma prior distribution
  on the hyperparameter. Default is 0.01.

- psi:

  (Optional) The shape parameter for the inverse-gamma prior
  distribution on the hyperparameter. Default is 0.01.

- burn.in:

  (optional) The number of iterations to discard as burn-in. Default is
  100.

- n.thin:

  (optional) The number of iterations to thin the MCMC samples by.
  Default is 1.

- verbose:

  (optional) A boolean indicating if progress should be printed to the
  console. Default is
  [`interactive()`](https://rdrr.io/r/base/interactive.html) therefore
  progress is shown if run interactively, but disabled in
  non-interactive contexts.

## Value

A `mcmc` object containing samples from the posterior distribution.

## Details

If `item.prior.var` is omitted, independent and identical N(0, 1^2)
prior distributions are placed on each object quality parameter.

If `lambda.initial` is ommitted, it is set to a vector of zeroes.

## Examples

``` r
# \donttest{
########################################
## Forced Marriage in Nottinghamshire ##
########################################
# Construct covariance matrix based on spatial information
prior.var <- expm::expm(forcedMarriage$adjacencyMatrix)
prior.var <- diag(diag(prior.var)^-0.5) %*% prior.var %*% diag(diag(prior.var)^-0.5)

# Fit model
# Using `n.iter = 3` here to reduce runtime, you will need more
# iterations for inference.
forcedMarriageModel <- speedyBBTm(
  outcome = rep(1, length(forcedMarriage$comparisons$win)),
  item1 = forcedMarriage$comparisons$win,
  item2 = forcedMarriage$comparisons$lost,
  item.prior.var = prior.var, n.iter = 3, burn.in = 0, verbose = FALSE
)

# Plot results

plot(forcedMarriageModel[, "lambda[1]"], xlab = "Iteration", ylab = expression(lambda[i]))

# }
```
