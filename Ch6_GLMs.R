### Chapter 6  - GLMs. bimon, logit, gamma ###
rm(list=ls())
dat<- read.csv('kubobook_2012/binomial/data4a.csv')
summary(dat)

library(ggplot2)
ggplot(dat, aes(x,y))+ 
  geom_point(aes(colour=f)) 
## --> This is a count data with lower and upper limit

# 6.3 Binomial distribution
df<- data.frame(x=0:8) # creating df b/c ggplot requires `data` input for multi-line viz
df$q10<-dbinom(df$x, 8, 0.1) # browseURL('https://stackoverflow.com/questions/16486819/how-to-deal-with-data-of-class-uneval-error-from-ggplot2')
df$q30<- dbinom(df$x,8,0.3)
df$q50<- dbinom(df$x, 8, 0.5)
df$q80<- dbinom(df$x,8, 0.8)
ggplot(df, aes(x)) +
  geom_line(aes(y=q10, colour='q=0.1')) + geom_point(aes(y=q10, colour='q=0.1')) +
  geom_line(aes(y=q30, colour='q=0.3')) + geom_point(aes(y=q30, colour='q=0.3')) +
  geom_line(aes(y=q50, colour='q=0.5')) + geom_point(aes(y=q50, colour='q=0.5')) +
  geom_line(aes(y=q80, colour='q=0.8')) + geom_point(aes(y=q80, colour='q=0.8')) +
  labs(x='Nr of times (out of 8)', y='probability') +
  NULL # Looking for a better way to combine `line` and `point`...

# 6.4 Logistic regression and logit link funciton
fitGlm<- glm(formula= cbind(y, N-y) ~ x+f, 
    data=dat,
    family = binomial(link='logit')) # link='logit' is default for binom
summary(fitGlm)
MASS::stepAIC(fitGlm)

# 6.6 Offset term in GLM
dat2<- read.csv('kubobook_2012/binomial/data4b.csv')
glm(y~x, offset = log(A), data=dat2, family='poisson') # see p.133 for more




