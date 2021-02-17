# Demonstrates how two distributions can have different estimated means despite being sampled from the same distribution
------------

## Setup
```r
devtools::install_github("jameshay218/lazymcmc")
library(lazymcmc)
library(ggplot2)
library(coda)
library(tidyverse)
```

Run the `priors_demonstration.R` script. This README talks through the rationale.

## Simulated data
Let's assume there are viruses, and we want to see if they have different means for one of their variables of interest (X). However, let's assume through simulation that in reality, they follow exactly the same distribution.

First, let's simulate the two sets of observations (virus A and virus B), assuming the same distribution. We have N=50 for virus A and N=7 for virus B. **NOTE** I have set a seed here, and other seeds may give different results and **CRUCICALLY** we may still accurately infer no difference in the means under some seeds.

## Priors
Assuming that the two viruses have different distributions for X, we want to estimate the means and standard deviations of X for each virus. Let's assume a normal prior for both means, with mean=15 and sd=5. For the standard deviations, assume exponentially distribution priors with rate=1.

## Posteriors
We estimate the posterior distribution of the distribution means for virus A and virus B. The posterior is a weighting of the prior and the likelihood, so with less data, the prior has a relatively bigger influence over the posterior.
