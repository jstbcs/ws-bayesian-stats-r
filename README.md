# Workshop on Bayesian Model Comparison in R

This repository contains materials for a workshop on Bayesian model comparison in R. There are two parts:

### Bayesian model comparison, part I
 
A key goal in research is to use data to assess competing hypotheses or theories. An alternative to the conventional significance testing is Bayesian model comparison. The main
idea is that competing theories are represented by statistical models. In the Bayesian framework, these models then yield predictions about data even before the data are seen.
How well the data match the predictions under competing models may be calculated, and the ratio of these matches—the Bayes factor—is used to assess the evidence for one model
compared to another. I will provide an introduction to Bayesian model comparison with the R-package BayesFactor. During the workshop we will discuss Bayesian t-test, ANOVA, and regression models. We will also talk about the dodgy topics such as placing prior distributions, conducting sensitivity analysis, and differences in philosophy between Bayesian and classical inference.
 
**How to Prepare**
Install the BayesFactor package

### Bayesian model comparison, part II

In the second part of the workshop we will talk about individual differences. Both in classical and Bayesian analysis individual differences are modeled as random effects. You will learn how to implement random effects with the BayesFactor package. I will also highlight advantages of Bayesian modeling of individual differences, namely that you get a posterior distribution for each individual in your data set. These individual effects can be used for additional inference. For example, you might ask whether everyone in your study showed an effect that is qualitatively similar or not (Haaf & Rouder, 2017). 
