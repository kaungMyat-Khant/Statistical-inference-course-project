---
title: "Simulation Exercise with Exponential distribution"
author: "Kaung Myat Khant"
date: "2025-02-23"
output:
  pdf_document:
    keep_md: TRUE
---



## Overview

In this exercise, there will be a demonstration of a simulation of a distribution of a small sample to approximate the population parameter. And the demonstration will use an exponential distribution.

## Simulation

Here, we will let the lambda (rate parameter of the distribution) be 0.2. For an exponential distribution, *mean = Standard deviation = 1/lambda*. Let's set the lambda = 0.2 for all the simulation and we will simulate the average of 40 exponential samples a thousand time.


``` r
lambda <- 0.2
n <- 40
set.seed(123) # for reproducibility
sim_vec <- NULL # A NULL vector to loop the simulation
for (i in 1:1000) { # loop for thousand time
    sim_vec <- c(sim_vec, mean(rexp(n, rate = lambda))) # average of exponential samples
}
head(sim_vec) # see the simulation
```

```
## [1] 4.811212 5.360077 4.592871 4.900051 5.516619 5.612835
```

The distribution of simulated data of 1000 are shown in the following histogram.


``` r
hist(sim_vec, 
     main = "Distribution of the simulated data", 
     xlab = "Sample means",
     col = "maroon")
```

![](simulation_exercise_files/figure-latex/histogram-1.pdf)<!-- --> 

## Sample mean versus Theorectical mean


``` r
## Calculate the means
sample_mean <- mean(sim_vec)
theoretical_mean <- 1/lambda
```

Here:\
- The sample mean from average of 1000 simulation is 5\
- Theoretical mean = 5.\
You can see that the sample mean is equal to theoretical mean.


``` r
par(mfrow = c(1,2), mar = c(5,2,2,2))
hist(sim_vec, main = "Sample mean", xlab = "Sample mean", col = "grey80")
abline(v = sample_mean, lwd = 2.5, lty= 1, col = "navy")
hist(rexp(40,0.2), main = "Theoretical mean", xlab = "Exponential samples", col = "grey80")
abline(v = theoretical_mean, lwd = 2.5, lty= 2, col = "darkgreen")
```

![](simulation_exercise_files/figure-latex/meanComparison-1.pdf)<!-- --> 

## Sample Variance vs Theoretical Variance


``` r
sample_variance <- var(sim_vec) # sample standard deviation
theoretical_variance <- 1/lambda^2/n # theoretical standard deviation
```

-   Sample variance = 0.6004928\
-   Theoretical variance = 0.625\
    Both the sample variance and theoretical variance are approximately 0.6

## Distribution

We will see if the distribution of simulated sample means are normal through visualization.


``` r
x <- seq(min(sim_vec), max(sim_vec), length.out = 100)
y <- dnorm(x, mean(sim_vec), sd(sim_vec))
hist(sim_vec, breaks = 100, freq = FALSE, axes = FALSE,
     main = "Probability density funtion of the sample means",
     xlab = "Sample mean")
lines(x,y, col = "blue", lwd = 2)
```

![Probabilty density function of the sample means with normal density curve](simulation_exercise_files/figure-latex/visualization-1.pdf) 

The figure shows that the sample means are distributed normally.  

