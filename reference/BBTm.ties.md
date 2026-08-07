# Bayesian inference for the Bradley-Terry model with ties

This function uses MCMC to sample from the posterior distribution of the
Bradley–Terry model with ties.A multivariate normal prior distribution
on the item quality parameters can be specified. An exponential prior
distribution is placed on the tie parameter theta, and a Metropolis-
Hasting random walk algorithm is used to update this parameter.

## Usage

``` r
BBTm.ties(
  n.objects,
  outcome,
  item1,
  item2,
  item.prior.var = NULL,
  theta.initial = NULL,
  lambda.initial = NULL,
  n.iter = 1000,
  hyperparameter = TRUE,
  chi = 0.01,
  psi = 0.01,
  rw.sd = 0.1,
  theta.rate = 0.01,
  burn.in = 100,
  n.thin = 1,
  verbose = interactive()
)
```

## Arguments

- n.objects:

  The number of objects in the study.

- outcome:

  Vector of outcomes. 0 if item 1 is the winner, 1 if item 2 is the
  winner, and 2 if it is a tie.

- item1:

  Vector of first items.

- item2:

  Vector of second items.

- item.prior.var:

  (optional) Matrix specifying the prior covariance of the item
  correlation parameters.

- theta.initial:

  (optional) Value of the tied parameter there for the first MCMC
  iteration.

- lambda.initial:

  (optional) Vector containing the values of the item parameters for the
  first MCMC iteration.

- n.iter:

  Number of MCMC samples to be drawn.

- hyperparameter:

  Boolean indicating if inference should be performed for the prior
  variance hyperparameter. If TRUE the prior variance (main diagonal of
  the covariance matrix) must be set to 1.

- chi:

  (optional) Rate parameter for the inverse-gamma prior distribution on
  the hyperparameter.

- psi:

  (optional) Shape parameter for the inverse-gamma prior distribution on
  the hyperparameter.

- rw.sd:

  (optional) Number describing the standard deviation of normal
  distribution proposal distribution for theta.

- theta.rate:

  (optional) The rate parameter of the exponential prior distribution
  placed on theta.

- burn.in:

  (optional) The number of iterations to use as a burn-in period.
  Default is 100.

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
containing samples from the posterior distribution

## Details

If `item.prior.var` is omitted, independent and identical N(0, 5^2)
prior distributions are placed on each object quality parameter.

If `lambda.initial` is omitted, it is set to a vector of zeroes.

## Examples

``` r
############################################
## Deprivation in Dar es Salaam, Tanzania ##
## Seymour et al (2022)                   ##
############################################
# Construct covariance matrix based on spatial informartion
prior.var <- expm::expm(darEsSalaam$adjacencyMatrix)
prior.var <- diag(diag(prior.var)^-0.5) %*% prior.var %*% diag(diag(prior.var)^-0.5)


# Fit BT model with ties
# Using `n.iter = 3` here to reduce model runtime, you will need
# a larger number of iterations for valid inference.
darTiedModel <- BBTm.ties(
  n.objects = nrow(darEsSalaam$adjacencyMatrix),
  outcome = darEsSalaam$comparisons$outcome,
  item1 = darEsSalaam$comparisons$subward1,
  item2 = darEsSalaam$comparisons$subward2,
  item.prior.var = prior.var,
  hyperparameter = TRUE, rw.sd = 0.005, n.iter = 3, burn.in = 0, verbose = FALSE
)

# Get posterior means
darTiedModelLambda <- parameter(darTiedModel, "lambda") - colMeans(parameter(darTiedModel, "lambda"))
lambda.mean <- rowMeans(darTiedModelLambda)

# Generate trace plots
plot(lambda.mean)

plot(parameter(darTiedModel, "theta"), type = "l")
```
