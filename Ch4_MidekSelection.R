### Chapter 4 - Model Selection in GLM ###
rm(list=ls())
dat<- read.csv('kubobook_2012/poisson/data3a.csv')

# 4.2 Deviance
fit<- glm(y~x, family=poisson,data = dat)
summary(fit)
## Deviance: Metric for badness of fit

### (Single model) deviance(D): -2* ML
dev<- logLik(fit)[1] * (-2); dev # [1] to extract ML value (remove other info e.g. df) 

### Full model deviane: deviance when #ofParams = #ofObservations,...
### ...i.e. One model for one set of observation
ML_full<- 0
for (i in 1:nrow(dat)){
  fitFull<-glm(y[i]~x[i], family=poisson, data=dat)
  ML_full<- ML_full + logLik(fitFull)[1]
}
devFull<- -2*ML_full
#### By definition, residual deviance is zero for full model

### Resid deviance: deviance - Full model Dev
dev - devFull; fit$deviance # both return the same value

### Null deviance: Deviance for the model with intercept only
fitNull <- glm(y~1, family = poisson, data=dat)
devNull <- logLik(fitNull)[1]*(-2) 
devNull - devFull; fitNull$deviance


# 4.3 AIC
## AIC = -2*(ML - nrParams(k) ) = Deviance + 2k
## --> Smaller AIC is desired
(-2)* (logLik(fit)[1] - length(coef(fit))); fit$aic

### Replicate Table 4.3 (P.77)
models<- c('null', 'f', 'x','x+f', 'full')
variables <- c('k', 'logL','deviance', 'residDev', 'AIC')
Table4.3=matrix(NA,5,5, dimnames = list(models, variables))
fitF<- glm(y~f, poisson, dat)
fitXF<- glm(y~x+f, poisson,dat)

model_options<- list(fitNull, fitF, fit, fitXF) # Investigate full model separetely
for (i in 1:4){
  model <- model_options[i][[1]]
  Table4.3[i,1] <- length(coef(model)) # Nr params
  Table4.3[i,2] <- logLik(model)[1] # Maxmul likelihood
  Table4.3[i,3] <- Table4.3[i,2]*(-2) # Deviance 
  Table4.3[i,4] <- Table4.3[i,3] - devFull # Residual Deviance
  Table4.3[i,5] <- Table4.3[i,3] + 2*Table4.3[i,1] # AIC
}
#### For full model
Table4.3[5,1]<-nrow(dat)
Table4.3[5,2]<- ML_full
Table4.3[5,3]<- devFull
Table4.3[5,4]<- devFull - devFull
Table4.3[5,5]<- devFull + 2*nrow(dat)

