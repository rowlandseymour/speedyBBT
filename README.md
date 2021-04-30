# speedyBBT <img src='man/figures/speedyBBT.png' align="right" height="140px" />
---
## Efficient Bayesian Inference for the Bradley--Terry Model
<!-- badges: start -->
<!-- badges: end -->
📦 The `speedyBBT` allows you to perform fast and efficient Bayesian inference for the Bradley--Terry model. The package estimates the object qualities usig a data augmentation method with Polya-Gamma prior distributions. 



## Installation
You can install `BSBT` by calling the following commands:
```{r}
devtools::install_github("rowlandseymour/BSBT", dependencies = TRUE) 

```


## Example

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

