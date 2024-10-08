# speedyBBT <img src='man/figures/speedyBBT.png' align="right" height="140px"/>
<!-- badges: start -->
  [![R-CMD-check](https://github.com/rowlandseymour/speedyBBT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rowlandseymour/speedyBBT/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->
---
## Efficient Bayesian Inference for the Bradley--Terry Model
📦 The `speedyBBT` R package allows you to perform fast and efficient Bayesian inference for the Bradley--Terry model. The package estimates the object qualities using a data augmentation method with Polya-Gamma prior distributions. This makes carrying out a Bayesian analysis of comparative judgement data quick and easy.


## Installation
You can install `speedyBBT` by calling the following commands:
```{r}
install.packages("speedyBBT")
#devtools::install_github("rowlandseymour/BSBT", dependencies = TRUE) #for development version
```



## Acknowledgements
This work is supported by the a UKRI Future Leaders Fellowship [MR/X034992/1], the Engineering and Physical Sciences Research Council [grant numbers EP/T003928/1, EP/R513283/1], the Economic and Social Sciences Research Council [ES/V015370/1], the Research England Policy Support Fund.

The Dar es Salaam comparative judgement dataset was collected by Madeleine Ellis, James Goulding, Bertrand Perrat, Gavin Smith and Gregor Engelmann. We gratefully acknowledge the Rights Lab at the University of Nottingham for supporting funding for the comprehensive ground truth survey. We also acknowledge Humanitarian Street Mapping Team (HOT) for providing a team of experts in data collection to facilitate the surveys. This fieldwork was also supported by the EPSRC Horizon Centre for Doctoral Training - My Life in Data (EP/L015463/1) and by EPSRC grant Neodemographics (EP/L021080/1).

The data in Nottinghamshire was collected with support from the Nottinghamshire Slavery Multi Agency Risk Assessment Conference. Data in South Yorkshire was collected with support from South Yorkshire Police. Data in Wokingham was collected with support from Wokingham Council. Data in Oxfordshire was collected with support from Oxford Against Cutting. Data in West Yorkshire was collected with support from West Yorkshire Police and Karma Nirvana. 
