#Overview
Binomial is an R package that implements functions for calculating probabilities of a Binomial random variable, and related calculations such as the probability distribution, the expected value, variance, etc.

The function bin_distribution gives us the probability distribution of success and can be plotted using the plot method.
The function bin_cumulative does the same thing but also includes the cumulative probability (adding to one in the last row). This can also be plotted.
We also make the function bin_variable that returns a "binvar" or binomial random variable object that is a list of the trials and probability. We can call the summary method to see even more information.
Lastly, we have the main functions for each of the summary measures like bin_mean and bin_variance

#Motivation
Understanding how to write a lot of these functions without using predefined R functions.

#Usage
```r
library(binomial)
library(ggplot2)

> bin_choose(n = 5, k = 2)
[1] 10
> bin_probability(success = 2, trials = 5, prob = 0.5)
[1] 0.3125
> bin_distribution(trials = 5, prob = 0.5)
  success probability
1       0     0.03125
2       1     0.15625
3       2     0.31250
4       3     0.31250
5       4     0.15625
6       5     0.03125
> dis1 <- bin_distribution(trials = 5, prob = 0.5)
> plot(dis1)
> bin_cumulative(trials = 5, prob = 0.5)
  success probability cumulative
1       0     0.03125    0.03125
2       1     0.15625    0.18750
3       2     0.31250    0.50000
4       3     0.31250    0.81250
5       4     0.15625    0.96875
6       5     0.03125    1.00000
> dis2 <- bin_cumulative(trials = 5, prob = 0.5)
> plot(dis2)
> bin1 <- bin_variable(trials = 10, p = 0.3)
> bin1
[1] "Binomial Variable"


Parameters
- number of trials: 10
- probability of success 0.3> bin1 <- bin_variable(trials = 10, p = 0.3)
> binsum1 <- summary(bin1)
> binsum1
[1] "Summary Binomial"


Parameters
- number of trials: 10
- probability of success: 0.3

Measures
- number of trials: 3
- number of trials: 2.1
- number of trials: 3
- number of trials: 0.2760262
- number of trials: -0.1238095> bin_mean(10, 0.3)
[1] 3
> bin_variance(10, 0.3)
[1] 2.1
> bin_mode(10, 0.3)
[1] 3
> bin_skewness(10, 0.3)
[1] 0.2760262
> bin_kurtosis(10, 0.3)
[1] -0.1238095
```
