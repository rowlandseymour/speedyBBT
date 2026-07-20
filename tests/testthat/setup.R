library(testthat)
library(Matrix)

# Set up test objects
test.iter <- 10
test.draws <- matrix(0, test.iter, 3)
test.mcmc.obj <- test.bad.obj <- coda::as.mcmc(
  x = test.draws,
  start = 1,
  end = test.iter,
  thin = 1
)
coda::varnames(test.mcmc.obj) <- c("lambda[1]", "lambda[2]", "alpha.sq")

coda::varnames(test.bad.obj) <- c("gamma[1]", "gamma[2]", "delta.sq")
