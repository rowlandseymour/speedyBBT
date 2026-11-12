# speedyBBT 2.0.0

This is a major release adding substantial new features, adding a `pkgdown` site, and
introducing changes to the function arguments of the model-fitting functions 
(`speedyBBTm()`, `BBTm()`, and `BBTm.ties()`) and many package improvements.

## Breaking changes

* Model-fitting functions have deprecated the `player1`, `player2`,
  and `player.prior.var` in favour of `item1`, `item2`, and `item.prior.var` in line with 
  web interface comparitive judgment tool.
* Model-fitting functions now accept arguments `n.thin` and `burn.in`,
 specifying the thinning rate and burn-in interval for MCMC sampling, respectively.
* Model-fitting functions now accept `verbose` argument that allows disabling the progress bar.

## New features

* Model-fitting functions now return an object of type ["mcmc"][coda::mcmc] allowing users
 use generic functions like `plot` and `summary` and access diagnostics and plotting functions
 from `coda` and `bayesplot`.
* `plot_qualities()` helper function generates a plot of the estimated 95% credible intervals for
 the $\lambda$ parameters estimated by the model-fitting functions.
* `parameter()` helper function introduced to extract model draws.
* Test coverage increased from 45% to 90%.
* New `pkgdown` website and "Getting started" vignette provided.

## Bug fixes

* The normalisation factor of the `BBTm()` function when no formula was provided has been corrected.
* Estimated quality parameters `$\lambda` from the `BBTm.ties()` function are now of the same orientation
 as other model-fitting functions, rather than transposed.

# speedyBBT 1.0.0

* First release of the `speedyBBT` package.
* Provides functions `speedyBBTm()` and `BBTm` to fit Bayesian Bradley-Terry functions
