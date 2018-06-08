### Chapter 3 - Generalised linear model ###

rm(list=ls())
dat<- read.csv('kubobook_2012/poisson/data3a.csv') # 3 col's. Nr of seeds, size and fertilisation(factor)

# 3.2 get an idea of the data
str(dat)
summary(dat)

# 3.3 Vidualisation
library(ggplot2)
# scatter plot (x and y)
ggplot(dat, aes(x,y)) + 
  geom_point(aes(colour=f)) +
  geom_smooth(method='glm', method.args=list(family='poisson'), se=F) + # see more below
  labs(x='x, size', y='y, nr seeds', colour='f, fertiliser') + 
  NULL #  Ending with NULL is useful when exploring data. browseURL('https://qiita.com/Atsushi776/items/3c5bb75b0b1fe4c2a543')
## y: non-negative integer, variance of y increases as E[y] increases

# box plot (f and y)
ggplot(dat, aes(f, y)) +
  geom_boxplot() +
  labs(x='x, fertiliser', y='y, nr of seeds') +
  NULL
## Result shows fertiliser has little effect

# 3.4 simple regression 
## Log link funciton: λ_i= exp(b_0 + b_1*x_i) --> log(λ)=b0 + b1*x
## Common link func when regressing poisson dist. 

### regress y(nr seeds) on x(size)
fit<- glm(y~x, data=dat,
          family = poisson(link='log')) # `link=log` by default
summary(fit) # z-val: Wald Statistics = Estim/SE
logLik(fit) # ML value. df=2 implies two params
## geom_smooth line indicates λ= exp(1.29 + 0.0757*x)

# 3.6 Multiple regression (num + fac)
fit_all<- glm(y~x+f, poisson, dat)
summary(fit_all)
## fT (coef for fertilised) is -0.032, indicating 1-exp(-0.032)= 3.15% decrease in mean...
logLik(fit_all) # slight improvement from `fit` (235.3863 to .2937)
## Rk: x+f actually means exp(x+f), so this is multiplication, exp(x)*exp(f)







