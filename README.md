# speedyBBT <img src="man/figures/speedyBBT.png" align="right" height="140px"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/rowlandseymour/speedyBBT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rowlandseymour/speedyBBT/actions/workflows/R-CMD-check.yaml) 

<!-- badges: end --> 

## Efficient Bayesian Inference for the Bradley--Terry Model

📦 The `speedyBBT` R package allows you to perform fast and efficient Bayesian inference for the Bradley--Terry model. The package estimates the object qualities using a data augmentation method with Polya-Gamma prior distributions. This makes carrying out a Bayesian analysis of comparative judgement data quick and easy.

## Installation

You can install `speedyBBT` by calling the following commands:

```{r}
install.packages("speedyBBT")
#devtools::install_github("rowlandseymour/BSBT", dependencies = TRUE) #for development version
```

## Usage
The code chunks below show how to use the package to fit the Bradley--Terry model
to a data set relating to forced marriage. Judges were shown pairs of wards and asked 
which had a higher rate of forced marriage. We can use the `speedyBBTm` function
to draw samples for the full conditional distributions for the ward quality parameters.
We place a multivariate normal prior distribution on the ward quality parameters. 
The covariance matrix of this prior distribution is contstucted using a network
representation of the wards in Nottinghamshire. 

```{r}
#View Data
data("forcedMarriage", package = "speedyBBT")
head(forcedMarriage$comparisons)


#Construct covariance matrix
expA  <- expm::expm(forcedMarriage$adjacencyMatrix)
sigma <- diag(diag(expA)^-0.5) %*% expA %*% diag(diag(expA)^-0.5)
    
#Fit model
forcedMarriageModel <- speedyBBTm(outcome = rep(1, length(forcedMarriage$comparisons$win)),
                                  player1 = forcedMarriage$comparisons$win, 
                                  player2 = forcedMarriage$comparisons$lost, 
                                  player.prior.var = sigma)
forcedMarriageModel$lambda  <- forcedMarriageModel$lambda - rowMeans(forcedMarriageModel$lambda)

#View Trace Plots
plot(forcedMarriageModel$lambda[, 10], type = 'l',
     xlab = "Iteration", ylab = expression(lambda[10]))
plot(forcedMarriageModel$lambda[, 20], type = 'l', 
     xlab = "Iteration", ylab = expression(lambda[20]))
plot(forcedMarriageModel$lambda[, 30], type = 'l', 
     xlab = "Iteration", ylab = expression(lambda[30]))
plot(forcedMarriageModel$lambda[, 40], type = 'l', 
     xlab = "Iteration", ylab = expression(lambda[40]))

plot(forcedMarriageModel$alpha.sq, type = 'l')

#View Results
forcedMarriageModelMeans <- colMeans(forcedMarriageModel$lambda[-c(1:50), ])
forcedMarriageModelLowerCI <- apply(forcedMarriageModel$lambda[-c(1:50), ], 2, quantile, 0.025)
forcedMarriageModelUpperCI <- apply(forcedMarriageModel$lambda[-c(1:50), ], 2, quantile, 0.975)
forcedMarriageResults <- data.frame("ward" = forcedMarriage$wards$NAME,
                                    "mean" = forcedMarriageModelMeans,
                                    "lowerCI" = forcedMarriageModelLowerCI, 
                                    "upperCI" = forcedMarriageModelUpperCI)
forcedMarriageResults <- forcedMarriageResults[order(forcedMarriageResults$mean), ]

plot(forcedMarriageResults$mean, xlab = "Ward", ylab = "Posterior Mean", ylim = c(-7.5, 7.5))
segments(x0 = 1:nrow(forcedMarriageResults), y0 = forcedMarriageResults$lowerCI, 
         y1 = forcedMarriageResults$upperCI)
```

## References

-   R. G. Seymour, A. Nyarko-Agyei, H. R. McCabe,K. Severn,D.Sirl, T. Kypraios, A. Taylor. (in press). Comparative Judgement Modeling to Map Forced Marriage at Local Levels. Annals of Applied Statistics.
-   [R. G. Seymour, D. Sirl, S. Preston, and J. Goulding. 2023. Multi-Level Spatial Comparative Judgement Models to Map Deprivation. Proceedings of the Joint Statistical Meeting 2023.](https://zenodo.org/records/8314257)
-   [R. G. Seymour, D. Sirl, S. Preston,\|. L. Dryden, B. Perrat, M. J. A. Ellis, and J. Goulding. 2022. The Bayesian Spatial Bradley-Terry model: Urban deprivation modelling in Tanzania. Journal of the Royal Statistical Society: C. 71 (2).](https://doi.org/10.1111/rssc.12532)

## Acknowledgements

This work is supported by the a UKRI Future Leaders Fellowship [MR/X034992/1], the Engineering and Physical Sciences Research Council [grant numbers EP/T003928/1, EP/R513283/1], the Economic and Social Sciences Research Council [ES/V015370/1], the Research England Policy Support Fund.

The Dar es Salaam comparative judgement dataset was collected by Madeleine Ellis, James Goulding, Bertrand Perrat, Gavin Smith and Gregor Engelmann. We gratefully acknowledge the Rights Lab at the University of Nottingham for supporting funding for the comprehensive ground truth survey. We also acknowledge Humanitarian Street Mapping Team (HOT) for providing a team of experts in data collection to facilitate the surveys. This fieldwork was also supported by the EPSRC Horizon Centre for Doctoral Training - My Life in Data (EP/L015463/1) and by EPSRC grant Neodemographics (EP/L021080/1).

The data in Nottinghamshire was collected with support from the Nottinghamshire Slavery Multi Agency Risk Assessment Conference. Data in South Yorkshire was collected with support from South Yorkshire Police. Data in Wokingham was collected with support from Wokingham Council. Data in Oxfordshire was collected with support from Oxford Against Cutting. Data in West Yorkshire was collected with support from West Yorkshire Police and Karma Nirvana.
