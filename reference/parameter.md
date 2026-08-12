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
# Using `n.iter = 3, burn.in = 0` to reduce runtime
# - you should use more iterations for reliable posterior estimates
forcedMarriageModel <- speedyBBTm(
  outcome = rep(1, length(forcedMarriage$comparisons$win)),
  player1 = forcedMarriage$comparisons$win,
  player2 = forcedMarriage$comparisons$lost,
  player.prior.var = prior.var, n.iter = 3, burn.in = 0
)
#>   |                                                                              |                                                                      |   0%

parameter(forcedMarriageModel, "lambda", c(10, 20, 30, 40))
#> Markov Chain Monte Carlo (MCMC) output:
#> Start = 1 
#> End = 3 
#> Thinning interval = 1 
#>       lambda[10] lambda[20]   lambda[30] lambda[40]
#> [1,] -0.04560956 0.03875247  0.004930672 0.04058178
#> [2,] -0.09374755 0.10272514 -0.040624428 0.09454545
#> [3,] -0.07072649 0.18630237 -0.011302126 0.09976397
parameter(forcedMarriageModel, "alpha.sq")
#> Markov Chain Monte Carlo (MCMC) output:
#> Start = 1 
#> End = 3 
#> Thinning interval = 1 
#>          alpha.sq
#> [1,] 0.0003000174
#> [2,] 0.0005719452
#> [3,] 0.0012691716
```
