---
title: "PS5_ECON_471"
author: "alynde2, kareem2, & luigi2"
date: "2023-03-19"
output:
  html_document:
    df_print: paged
  extra_dependencies: amsmath
  keep_tex: yes
  pdf_document: null
---

```{r echo = TRUE, results='asis'}
rm(list = ls())
library(data.table)
library(ggplot2)
set.seed(1)
setwd("/Users/aidanlynde/Downloads")
data <- fread("population.csv", header = TRUE)
```

##### 1

The Central Limit Theorem (CLT) states that the sample mean of a sufficiently large random sample taken from a population with a finite mean and variance will be approximately normally distributed, regardless of the underlying distribution of the population. More formally, if X1, X2, ..., Xn are independent and identically distributed random variables from a population with mean μ and variance σ^2, then the sample mean X_bar will have an asymptotic normal distribution with mean μ and variance σ^2/n as n approaches infinity.

##### 2

Sebastian, Kiana -- Age: 40
Schauss, Derek -- Age: 39
Mathis, Ryan -- Age: 39
Average = 39.3333

##### 3

Mace, Casey -- Age: 29
Brock, Kelsey -- Age: 25
Chen, Nathaniel -- Age: 48
Average = 34

##### 4

If we sample with replacement, each member of the population can be chosen multiple times, and there are no restrictions on which members can be chosen in each draw.
Therefore, the number of possible samples of size n = 3 is equal to the number of ways we can choose 3 elements from the population, with replacement allowed.
Assuming the population size is denoted by N, the number of possible samples of size n = 3, with replacement allowed, is given by the formula:
N^3

```{r echo = TRUE, results='asis'}
## 5. Plotting Histograms of male, age, and grad
    ## Create a histogram of age
pdf(file="hist-age.pdf", width = 8, height = 4)
hist(population$age,
     xlab = "Age",
     ylab = "Frequency",
     main = "Histogram of age")
dev.off()

    ## Create a histogram of male
pdf(file="hist-male.pdf", width = 8, height = 4)
hist(population$male,
     xlab = "Male",
     ylab = "Frequency",
     main = "Histogram of male")
dev.off()

    ## Create a histogram of grad
pdf(file="hist-grad.pdf", width = 8, height = 4)
hist(population$grad,
     xlab = "Grad",
     ylab = "Frequency",
     main = "Histogram of grad")
dev.off()
    ## Create a histogram of age
ggplot(population) +
  geom_histogram(mapping = aes(x = age)) +
  labs(x = "Age",
       y = "Frequency",
       title = "Histogram of age") +
  theme_minimal()
ggsave("hist-age-gg.pdf", width = 8, height = 4)

    ## Create a histogram of male
ggplot(population) +
  geom_histogram(mapping = aes(x = male)) +
  labs(x = "Male",
       y = "Frequency",
       title = "Histogram of male") +
  theme_minimal()
ggsave("hist-male-gg.pdf", width = 8, height = 4)

    ## Create a histogram of grad
ggplot(population) +
  geom_histogram(mapping = aes(x = grad)) +
  labs(x = "Grad",
       y = "Frequency",
       title = "Histogram of grad") +
  theme_minimal()
ggsave("hist-grad-gg.pdf", width = 8, height = 4)



## 6. Write a function that samples the population. 
sampler <- function(population, n) {
  sample.rows <- sample(nrow(population), n, replace=TRUE)
  return(population[sample.rows,])
}



## 7. Function that determines mean of a variable given a sample
    ## Function that samples n individuals from the population with     replacement
sampler <- function(population, n) {
  sample.rows <- sample(nrow(population), n, replace = TRUE)
  return(population[sample.rows, ])
}

    ## Function that returns the mean of a variable in the sample
sample.mean <- function(sample, variable) {
  return(mean(sample[, variable]))
}




## 8. Write a loop and verify the strong law of large numbers.
n <- 200
k <- 30

sample_means <- numeric(n)
sample_size <- 0

for (i in 1:n) {
  x <- rchisq(n = 1, df = k)
  sample_size <- sample_size + 1
  sample_means[sample_size] <- mean(x)
}

plot(x = 1:n,
     y = sample_means,
     type = "l",
     xlab = "Sample size",
     ylab = "Sample mean",
     main = "Sample mean as sample size grows")

## 9. Now let's see how the sampling distribution evolves

library(dplyr)

n_list <- c(1, 5, 10, 50, 100)

sample.dist <- function(iters, population, n, variable) {
  
  sample_means <- numeric(iters)
  
  for (i in seq_len(iters)) {
    
    sample <- population %>%
      slice_sample(n)
    
    sample_mean <- mean(sample[[variable]])
    
    sample_means[i] <- sample_mean
  }
  
  return(sample_means)
}

plot.hist <- function(sample_means, n, variable) {
  
  xlim <- c(0, ifelse(variable == "age", 70, 1))
  
  hist(sample_means,
       main = paste("Sample size:", n, "Variable:", variable),
       xlab = "Sample mean",
       xlim = xlim,
       breaks = 30,
       col = "lightblue",
       border = "white")
}

for (n in n_list) {
  
  sample_data <- data %>%
    slice_sample(n)
  
  age_sample_dist <- sample.dist(iters = 2000, population = sample_data, n = n, variable = "age")
  grad_sample_dist <- sample.dist(iters = 2000, population = sample_data, n = n, variable = "grad")
  male_sample_dist <- sample.dist(iters = 2000, population = sample_data, n = n, variable = "male")
  
  plot.hist(age_sample_dist, n = n, variable = "age")
  plot.hist(grad_sample_dist, n = n, variable = "grad")
  plot.hist(male_sample_dist, n = n, variable = "male")
}

```