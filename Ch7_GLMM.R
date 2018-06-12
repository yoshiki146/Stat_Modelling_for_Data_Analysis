### Chapter 7 - GLMM ###
rm(list=ls())
dat<- read.csv('kubobook_2012/glmm/data.csv')

# 7.1 The data with idiosyncracy
glm<- glm(formula= cbind(y,N-y)~x, data = dat, family = binomial)

## Figure 7.3
library(ggplot2)
ggplot(dat, aes(x,y)) + 
  geom_jitter(alpha=0.8, shape=1,
              position=position_jitter(width=0.1,height=0)) 

q<- psych::logistic(x=(glm$coef[1] + glm$coef[2]*4))

pred<- dbinom(0:8,8,q)*20 # 20 is nr of observation where x==4
plot(table(dat[dat$x==4,]$y), type='p', ylim = c(0,6),
     xlab= 'nr of seeds y (x==4)', ylab='nr of observations')
lines(0:8, pred, type='b', pch=16)
### Prediction is poor <-- failed to catch individual differences (need to model em. --> GLMM)

# 7.4 Estimation for GLMM
glmmML::glmmML(cbind(y, N-y)~x, data=dat, family = binomial,
              cluster = id)
