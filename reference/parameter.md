# Extract parameter draws from a model object

This function helps the user extract relevant estimates from parameters
fit using a Bayesian Bradley-Terry model.

## Usage

``` r
parameter(model_output, parameter_name, indices_to_extract = NULL)
```

## Arguments

- model_output:

  The mcmc object that contains the draws output from the BBT model.

- parameter_name:

  The name of the parameter to extract draws for.

- indices_to_extract:

  A single column index or a vector of column indices to extract from
  the draws matrix.

## Value

A vector containing the draws of the parameter.

## Examples

``` r

prior.var <- expm::expm(forcedMarriage$adjacencyMatrix)
prior.var <- diag(diag(prior.var)^-0.5) %*% prior.var %*% diag(diag(prior.var)^-0.5)

# Fit model
# Using `n.iter = 3` here to reduce runtime, you will need more
# iterations for inference.
forcedMarriageModel <- speedyBBTm(
  outcome = rep(1, length(forcedMarriage$comparisons$win)),
  item1 = forcedMarriage$comparisons$win,
  item2 = forcedMarriage$comparisons$lost,
  item.prior.var = prior.var, n.iter = 3, burn.in = 1, verbose = FALSE
)

parameter(forcedMarriageModel, "lambda", c(10, 20, 30, 40))
#> Markov Chain Monte Carlo (MCMC) output:
#> Start = 1 
#> End = 2 
#> Thinning interval = 1 
#>       lambda[10] lambda[20]  lambda[30] lambda[40]
#> [1,] -0.09374755  0.1027251 -0.04062443 0.09454545
#> [2,] -0.07072649  0.1863024 -0.01130213 0.09976397
parameter(forcedMarriageModel, "alpha.sq")
#> Markov Chain Monte Carlo (MCMC) output:
#> Start = 1 
#> End = 2 
#> Thinning interval = 1 
#>          alpha.sq
#> [1,] 0.0005719452
#> [2,] 0.0012691716
```
