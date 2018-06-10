
### Chapter 5, Likelihood Ratio Test ###
rm(list=ls())
dat<- read.csv('kubobook_2012/poisson/data3a.csv')

# 5.4.1 Paarametric Bootstrap
## compute difference of deviance b/w y~1 n y~x 
diffDev <- function(data){   
  # generate data
  nrSample <- nrow(data)
  meanSample <- mean(data$y)
  data$yRand <- rpois(n= nrSample, lambda = meanSample) # Randomly generate y based on poission dist
  #evaluate DD
  fit1<- glm(yRand~1, poisson, data)
  fit2<- glm(yRand~x, poisson, data)
  DD <- fit1$deviance - fit2$deviance
  return(DD)
}

## Repeat n times
dd12<- replicate(n=10000, diffDev(dat)) # replicate is faster than for-loop

hist(dd12,100); abline(v=4.5, lty=2) # 4.5 is DD for y (observed)
sum(dd12>=4.5)
sum(dd12>=4.5)/length(dd12) # bootstrapping-based p-value
quantile(dd12, 0.95)

# 5.4.2 Approximation using Chisq dist
fit1 <- glm(y~1, poisson, dat)
fit2<- glm(y~x, poisson, dat)
anova(fit1, fit2, test='Chisq') # Analysis of deviance 
