# Generalised Bradley-Terry model

This function fits the Bradley-Terry model with comparison and item
specific effects. Each comparison can be assigned a real value to allow
for a specific effect for the comparison, such as bias, ordering or
home/away effect. The value of this effect is denoted kappa. The item
specific effects are described through a formula and data.frame
containing the value. The function places a normal prior distribution on
both kappa and the item specific parameters beta.

## Usage

``` r
BBTm(
  outcome,
  item1,
  item2,
  lambda.initial = NULL,
  item.prior.var = NULL,
  beta.initial = NULL,
  n.iter = 1000,
  formula = NULL,
  data = NULL,
  advantage = NULL,
  kappa.initial = NULL,
  kappa.var = NULL,
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

  Vector of outcomes. 1 if item2 is the winner, 0 if item1 is the
  winner.

- item1:

  Vector of first items.

- item2:

  Vector of second items.

- lambda.initial:

  (optional) Vector containing the values of the item parameters for the
  first MCMC iteration.

- item.prior.var:

  (optional) Matrix specifying the prior covariance of the item
  correlation parameters.

- beta.initial:

  (optional) Vector containing the values of the item specific
  parameters for the first MCMC iteration.

- n.iter:

  Number of MCMC samples to be drawn.

- formula:

  Formula with no left-hand-side specifying the item specific effects.

- data:

  Data.frame with a row corresponding to each item and column
  corresponding to each covariate.

- advantage:

  (optional) A vector with the value of the comparisons specific effect
  for each comparison.

- kappa.initial:

  (optional) An initial value for the comparison specific value kappa.

- kappa.var:

  (optional) The prior variance of the comparison specific value kappa.

- hyperparameter:

  Boolean indicating if inference should be performed for the prior
  variance hyperparameter. If TRUE the prior variance (main diagonal of
  the covariance matrix) must be set to 1.

- chi:

  Rate parameter for the inverse-gamma prior distribution on the
  hyperparameter.

- psi:

  Shape parameter for the inverse-gamma prior distribution on the
  hyperparameter.

- burn.in:

  The number of iterations to use for a burn.in, default is 100.

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

A [`coda::mcmc`](https://rdrr.io/pkg/coda/man/mcmc.html) object
containing samples from the posterior distribution.

## Details

If `item.prior.var` is omitted, independent and identical N(0, 5^2)
prior distributions are placed on each object quality parameter.

If `beta.initial`is omitted, it is set to a vector of zeroes.

If `kappa.var` is omitted, it is set to N(0, 5^2), if `kappa.initial` is
omitted it is set to 0.5.

## Examples

``` r
# \donttest{
#####################
## Wimbledon 2019 ##
####################
# Fit model where the quality of each player depends on their rank
# and the number of points they had immediately before the tournament.
# Allow an effect for a match being in the first or second week.
wimbledonModel <- BBTm(
  outcome = wimbledon$matches$outcome,
  item2 = wimbledon$matches$loser,
  item1 = wimbledon$matches$winner,
  advantage = wimbledon$matches$secondWeek,
  formula = ~ rank + points,
  data = wimbledon$players,
  n.iter = 1000, verbose = FALSE
)

# Plot posterior distributions
hist(parameter(wimbledonModel, "kappa"), main = "", xlab = expression(kappa), freq = FALSE)

# }
```
