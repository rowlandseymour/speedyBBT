# speedyBBT <img src='man/figures/speedyBBT.png' align="right" height="140px"/>
---
## Efficient Bayesian Inference for the Bradley--Terry Model
<!-- badges: start -->
[![Travis build status](https://travis-ci.com/rowlandseymour/speedyBBT.svg?branch=master)](https://travis-ci.com/rowlandseymour/speedyBBT)
<!-- badges: end -->
📦 The `speedyBBT` allows you to perform fast and efficient Bayesian inference for the Bradley--Terry model. The package estimates the object qualities using a data augmentation method with Polya-Gamma prior distributions. 



## Installation
You can install `BSBT` by calling the following commands:
```{r}
devtools::install_github("rowlandseymour/speedyBBT", dependencies = TRUE) 

```

## Example
To analyse the comparative judgement data set on deprivation in Dar es Salaam, Taznania, use the following code. 
``` r
library(speedyBBT)
library(BSBT)
#Read in comparative judgement data set
data("dar.comparisons")

#Prepare data
N <- 452
win.matrix <- comparisons_to_matrix(N, dar.comparisons[, 1:2])
X <- construct.design.matrix(N)
y <- win.matrix[lower.tri(win.matrix)]
n <- (win.matrix + t(win.matrix))[lower.tri(win.matrix)]

#Construct covariance matrix
k <- constrained_adjacency_covariance_function(dar.adj.matrix, "matrix", 1, linear.combination = rep(1, N))

#Run MCMC
mcmc.samples <- gibbs.bt(N, y, n, X, k$inv, rep(0, N), hyperparameter = TRUE)

#Compute posterior medians
lambda <- apply(mcmc.samples$lambda[-c(1:50), ], 2, median)
```

